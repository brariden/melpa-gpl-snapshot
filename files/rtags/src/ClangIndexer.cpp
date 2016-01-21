/* This file is part of RTags (http://rtags.net).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#define RTAGS_SINGLE_THREAD
#include "ClangIndexer.h"

#include <unistd.h>
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 25)
#include <clang-c/Documentation.h>
#endif

#include "Diagnostic.h"
#include "FileMap.h"
#include "QueryMessage.h"
#include "RClient.h"
#include "rct/Connection.h"
#include "rct/EventLoop.h"
#include "rct/SHA256.h"
#include "RTags.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"

const CXSourceLocation ClangIndexer::nullLocation = clang_getNullLocation();
const CXCursor ClangIndexer::nullCursor = clang_getNullCursor();

static inline String usr(const CXCursor &cursor)
{
    return RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(cursor)));
}

struct VerboseVisitorUserData {
    int indent;
    String out;
    ClangIndexer *indexer;
};

static inline void setType(Symbol &symbol, const CXType &type)
{
    symbol.type = type.kind;
    const String str = RTags::eatString(clang_getTypeSpelling(type));
    symbol.typeName = str;
}

Flags<Server::Option> ClangIndexer::sServerOpts;
ClangIndexer::ClangIndexer()
    : mClangUnit(0), mIndex(0), mLastCursor(nullCursor), mVisitFileResponseMessageFileId(0),
      mVisitFileResponseMessageVisit(0), mParseDuration(0), mVisitDuration(0),
      mBlocked(0), mAllowed(0), mIndexed(1), mVisitFileTimeout(0),
      mIndexDataMessageTimeout(0), mFileIdsQueried(0), mFileIdsQueriedTime(0),
      mCursorsVisited(0), mLogFile(0), mConnection(Connection::create(RClient::NumOptions)),
      mUnionRecursion(false)
{
    mConnection->newMessage().connect(std::bind(&ClangIndexer::onMessage, this,
                                                std::placeholders::_1, std::placeholders::_2));
}

ClangIndexer::~ClangIndexer()
{
    if (mLogFile)
        fclose(mLogFile);
    if (mClangUnit)
        clang_disposeTranslationUnit(mClangUnit);
    if (mIndex)
        clang_disposeIndex(mIndex);
}

bool ClangIndexer::exec(const String &data)
{
    Deserializer deserializer(data);
    uint16_t protocolVersion;
    deserializer >> protocolVersion;
    if (protocolVersion != RTags::DatabaseVersion) {
        error("Wrong protocol %d vs %d", protocolVersion, RTags::DatabaseVersion);
        return false;
    }
    uint64_t id;
    String socketFile;
    Flags<IndexerJob::Flag> indexerJobFlags;
    uint32_t connectTimeout, connectAttempts;
    int32_t niceValue;
    Hash<uint32_t, Path> blockedFiles;

    deserializer >> id;
    deserializer >> socketFile;
    deserializer >> mProject;
    deserializer >> mSource;
    deserializer >> mSourceFile;
    deserializer >> indexerJobFlags;
    deserializer >> mVisitFileTimeout;
    deserializer >> mIndexDataMessageTimeout;
    deserializer >> connectTimeout;
    deserializer >> connectAttempts;
    deserializer >> niceValue;
    deserializer >> sServerOpts;
    deserializer >> mUnsavedFiles;
    deserializer >> mDataDir;
    deserializer >> mDebugLocations;
    deserializer >> blockedFiles;

#if 0
    while (true) {
        FILE *f = fopen((String("/tmp/stop_") + mSourceFile.fileName()).constData(), "r+");
        if (f) {
            fseek(f, 0, SEEK_END);
            fprintf(f, "Waiting ... %d\n", getpid());
            fclose(f);
            sleep(1);
        } else {
            break;
        }
    }
#endif

    const uint64_t parseTime = Rct::currentTimeMs();

    if (niceValue != INT_MIN) {
        errno = 0;
        if (nice(niceValue) == -1) {
            error() << "Failed to nice rp" << Rct::strerror();
        }
    }

    if (mSourceFile.isEmpty()) {
        error("No sourcefile");
        return false;
    }
    if (!mSource.fileId) {
        error("Bad fileId");
        return false;
    }

    if (mProject.isEmpty()) {
        error("No project");
        return false;
    }

    Location::init(blockedFiles);
    Location::set(mSourceFile, mSource.fileId);
    while (true) {
        if (mConnection->connectUnix(socketFile, connectTimeout))
            break;
        if (!--connectAttempts) {
            error("Failed to connect to rdm on %s (%dms timeout)", socketFile.constData(), connectTimeout);
            return false;
        }
        usleep(500 * 1000);
    }
    // mLogFile = fopen(String::format("/tmp/%s", mSourceFile.fileName()).constData(), "w");
    mIndexDataMessage.setProject(mProject);
    mIndexDataMessage.setIndexerJobFlags(indexerJobFlags);
    mIndexDataMessage.setParseTime(parseTime);
    mIndexDataMessage.setKey(mSource.key());
    mIndexDataMessage.setId(id);

    assert(mConnection->isConnected());
    mIndexDataMessage.files()[mSource.fileId] |= IndexDataMessage::Visited;
    parse() && visit() && diagnose();
    String message = mSourceFile.toTilde();
    String err;
    StopWatch sw;
    int writeDuration = -1;
    if (!mClangUnit || !writeFiles(RTags::encodeSourceFilePath(mDataDir, mProject, 0), err)) {
        message += " error";
        if (!err.isEmpty())
            message += (' ' + err);
    } else {
        writeDuration = sw.elapsed();
    }
    message += String::format<16>(" in %lldms. ", mTimer.elapsed());
    int cursorCount = 0;
    int symbolNameCount = 0;
    for (const auto &unit : mUnits) {
        cursorCount += unit.second->symbols.size();
        symbolNameCount += unit.second->symbolNames.size();
    }
    if (mClangUnit) {
        String queryData;
        if (mFileIdsQueried)
            queryData = String::format(", %d queried %dms", mFileIdsQueried, mFileIdsQueriedTime);
        const char *format = "(%d syms, %d symNames, %d includes, %d of %d files, symbols: %d of %d, %d cursors%s%s) (%d/%d/%dms)";
        message += String::format<1024>(format, cursorCount, symbolNameCount,
                                        mIndexDataMessage.includes().size(), mIndexed,
                                        mIndexDataMessage.files().size(), mAllowed,
                                        mAllowed + mBlocked, mCursorsVisited,
                                        queryData.constData(), mIndexDataMessage.flags() & IndexDataMessage::UsedPCH ? ", pch" : "",
                                        mParseDuration, mVisitDuration, writeDuration);
    }
    if (mIndexDataMessage.indexerJobFlags() & IndexerJob::Dirty)
        message += " (dirty)";

    mIndexDataMessage.setMessage(message);
    sw.restart();
    if (!mConnection->send(mIndexDataMessage)) {
        error() << "Couldn't send IndexDataMessage" << mSourceFile;
        return false;
    }
    mConnection->finished().connect(std::bind(&EventLoop::quit, EventLoop::eventLoop()));
    if (EventLoop::eventLoop()->exec(mIndexDataMessageTimeout) == EventLoop::Timeout) {
        error() << "Timed out sending IndexDataMessage" << mSourceFile;
        return false;
    }
    if (getenv("RDM_DEBUG_INDEXERMESSAGE"))
        error() << "Send took" << sw.elapsed() << "for" << mSourceFile;

    return true;
}

void ClangIndexer::onMessage(const std::shared_ptr<Message> &msg, const std::shared_ptr<Connection> &/*conn*/)
{
    assert(msg->messageId() == VisitFileResponseMessage::MessageId);
    const std::shared_ptr<VisitFileResponseMessage> vm = std::static_pointer_cast<VisitFileResponseMessage>(msg);
    mVisitFileResponseMessageVisit = vm->visit();
    mVisitFileResponseMessageFileId = vm->fileId();
    assert(EventLoop::eventLoop());
    EventLoop::eventLoop()->quit();
}

Location ClangIndexer::createLocation(const Path &sourceFile, unsigned int line, unsigned int col, bool *blockedPtr)
{
    uint32_t id = Location::fileId(sourceFile);
    Path resolved;
    if (!id) {
        bool ok;
        for (int i=0; i<4; ++i) {
            resolved = sourceFile.resolved(Path::RealPath, Path(), &ok);
            // if ok is false it means the file is gone, in case this happens
            // during a git pull or something we'll give it a couple of chances.
            if (ok)
                break;
            usleep(50000);
        }
        if (!ok)
            return Location();
        id = Location::fileId(resolved);
        if (id)
            Location::set(sourceFile, id);
    }
    assert(!resolved.contains("/../"));

    if (id) {
        if (blockedPtr) {
            Hash<uint32_t, Flags<IndexDataMessage::FileFlag> >::iterator it = mIndexDataMessage.files().find(id);
            if (it == mIndexDataMessage.files().end()) {
                // the only reason we already have an id for a file that isn't
                // in the mIndexDataMessage.mFiles is that it's blocked from the outset.
                // The assumption is that we never will go and fetch a file id
                // for a location without passing blockedPtr since any reference
                // to a symbol in another file should have been preceded by that
                // header in which case we would have to make a decision on
                // whether or not to index it. This is a little hairy but we
                // have to try to optimize this process.
#ifndef NDEBUG
                if (resolved.isEmpty())
                    resolved = sourceFile.resolved();
#endif
                mIndexDataMessage.files()[id] = IndexDataMessage::NoFileFlag;
                *blockedPtr = true;
                return Location();
            } else if (!it->second) {
                *blockedPtr = true;
                return Location();
            }
        }
        return Location(id, line, col);
    }

    ++mFileIdsQueried;
    VisitFileMessage msg(resolved, mProject, mIndexDataMessage.key());

    mVisitFileResponseMessageFileId = UINT_MAX;
    mVisitFileResponseMessageVisit = false;
    mConnection->send(msg);
    StopWatch sw;
    EventLoop::eventLoop()->exec(mVisitFileTimeout);
    const int elapsed = sw.elapsed();
    mFileIdsQueriedTime += elapsed;
    switch (mVisitFileResponseMessageFileId) {
    case 0:
        return Location();
    case UINT_MAX:
        // timed out.
        if (mVisitFileResponseMessageFileId == UINT_MAX) {
            error() << "Error getting fileId for" << resolved << mLastCursor
                    << elapsed << mVisitFileTimeout;
        }
        exit(1);
    default:
        id = mVisitFileResponseMessageFileId;
        break;
    }
    Flags<IndexDataMessage::FileFlag> &flags = mIndexDataMessage.files()[id];
    if (mVisitFileResponseMessageVisit) {
        flags |= IndexDataMessage::Visited;
        ++mIndexed;
    }
    // fprintf(mLogFile, "%s %s\n", file.second ? "WON" : "LOST", resolved.constData());

    Location::set(resolved, id);
    if (resolved != sourceFile)
        Location::set(sourceFile, id);

    if (blockedPtr && !mVisitFileResponseMessageVisit) {
        *blockedPtr = true;
        return Location();
    }
    return Location(id, line, col);
}

static inline void tokenize(const char *buf, int start,
                            int *templateStart, int *templateEnd,
                            int *sectionCount, int sections[1024])
{
    int templateCount = 0;
    *templateStart = *templateEnd = -1;
    *sectionCount = 1;
    sections[0] = start;
    int functionStart = -1;
    int functionEnd = -1;

    int idx = start;
    while (true) {
        switch (buf[++idx]) {
        case '<':
            if (buf[idx + 1] == '<') {
                ++idx;
            } else if (functionStart == -1 && (idx - 8 < 0 || strncmp("operator", buf + idx - 8, 8) != 0)) {
                if (!templateCount++)
                    *templateStart = idx;
            }
            break;
        case '>':
            if (buf[idx + 1] == '>') {
                ++idx;
            } else if (functionStart == -1 && (idx - 8 < 0 || strncmp("operator", buf + idx - 8, 8) != 0)) {
                if (!--templateCount)
                    *templateEnd = idx;
            }
            break;
        case '(':
            if (!templateCount)
                functionStart = idx;
            break;
        case ')':
            if (!templateCount)
                functionEnd = idx;
            break;
        case ':':
            if (!templateCount && (functionStart == -1 || functionEnd != -1) && buf[idx + 1] == ':' && (*sectionCount) < 1024) {
                sections[(*sectionCount)++] = idx + 2;
                ++idx;
            }
            break;
        case '\0':
            return;
        }
    }
}

String ClangIndexer::addNamePermutations(const CXCursor &cursor, const Location &location,
                                         RTags::CursorType cursorType)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    const CXCursorKind originalKind = kind;
    char buf[1024 * 512];
    int pos = sizeof(buf) - 1;
    buf[pos] = '\0';
    int cutoff = -1;

    CXCursor c = cursor;
    do {
        CXStringScope displayName(clang_getCursorDisplayName(c));
        const char *name = displayName.data();
        if (!name)
            break;
        const int len = strlen(name);
        if (!len)
            break;

        if (pos != sizeof(buf) - 1) {
            pos -= 2;
            if (pos >= 0)
                memset(buf + pos, ':', 2);
        }
        pos -= len;
        if (pos < 0) {
            error("SymbolName too long. Giving up");
            return String();
        }
        memcpy(buf + pos, name, len);

        c = clang_getCursorSemanticParent(c);
        kind = clang_getCursorKind(c);
        if (cutoff == -1) {
            switch (kind) {
            case CXCursor_ClassDecl:
            case CXCursor_ClassTemplate:
            case CXCursor_StructDecl:
                break;
            case CXCursor_Namespace:
                // namespaces can include all namespaces in their symbolname
                if (originalKind == CXCursor_Namespace)
                    break;
            default:
                cutoff = pos;
                break;
            }
        }
    } while (RTags::needsQualifiers(kind));

    if (static_cast<size_t>(pos) == sizeof(buf) - 1) {
        return String();
    }
    String type;
    switch (originalKind) {
    case CXCursor_StructDecl:
        type = "struct ";
        break;
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
        type = "class ";
        break;
    case CXCursor_Namespace:
        type = "namespace ";
        break;
    case CXCursor_Destructor:
    case CXCursor_Constructor:
        break;
    default: {
        type = RTags::eatString(clang_getTypeSpelling(clang_getCursorType(cursor)));
        const int paren = type.indexOf('(');
        if (paren != -1) {
            type.resize(paren);
        } else if (!type.isEmpty() && !type.endsWith('*') && !type.endsWith('&')) {
            type.append(' ');
        }
        break; }
    }

    if (cutoff == -1)
        cutoff = pos;

    String ret;
    if (!type.isEmpty()) {
        ret = type;
        ret.append(buf + cutoff, std::max<int>(0, sizeof(buf) - cutoff - 1));
    } else {
        ret.assign(buf + cutoff, std::max<int>(0, sizeof(buf) - cutoff - 1));
    }
    if (cursorType == RTags::Type_Reference) {
        return ret;
    }

    int templateStart, templateEnd, colonColonCount;
    int colonColons[1024];
    ::tokenize(buf, pos,
               &templateStart, &templateEnd,
               &colonColonCount, colonColons);

    // i == 0 --> with templates,
    // i == 1 without templates or without EnumConstantDecl part
    for (int i=0; i<2; ++i) {
        for (int j=0; j<colonColonCount; ++j) {
            const char *ch = buf + colonColons[j];
            const String name(ch, std::max<int>(0, sizeof(buf) - (ch - buf) - 1));
            if (name.isEmpty())
                continue;
            unit(location.fileId())->symbolNames[name].insert(location);
            if (!type.isEmpty() && (originalKind != CXCursor_ParmDecl || !strchr(ch, '('))) {
                // We only want to add the type to the final declaration for ParmDecls
                // e.g.
                // void foo(int)::bar
                // and
                // int bar
                //
                // not
                // int void foo(int)::bar
                // or
                // void foo(int)::int bar

                unit(location.fileId())->symbolNames[type + name].insert(location);
            }
        }

        if (i == 1 || (templateStart == -1 && originalKind != CXCursor_EnumConstantDecl)) {
            // nothing more to do
            break;
        }

        if (originalKind == CXCursor_EnumConstantDecl) { // remove CXCursor_EnumDecl
            // struct A { enum B { C } };
            // Will by default generate a A::B::C symbolname.
            // This code removes the B:: part from it
            if (colonColonCount > 2) {
                const char *last = buf + colonColons[colonColonCount - 1];
                const char *secondLast = buf + colonColons[colonColonCount - 2];
                const int len = (last - secondLast);
                memmove(buf + pos + len, buf + pos, secondLast - (buf + pos));
                pos += len;
            }
        } else { // remove templates
            assert(templateStart != -1);
            assert(templateEnd != -1);
            const int templateSize = (templateEnd - templateStart) + 1;
            memmove(buf + pos + templateSize, buf + pos, (buf + templateStart) - (buf + pos));
            pos += templateSize;
        }
        // ### We could/should just move the colon colon values but this
        // should be pretty quick and I don't want to write the code to
        // do it.
        ::tokenize(buf, pos,
                   &templateStart, &templateEnd,
                   &colonColonCount, colonColons);
    }

    return ret;
}


static inline CXCursor findDestructorForDelete(const CXCursor &deleteStatement)
{
    const CXCursor child = RTags::findFirstChild(deleteStatement);
    CXCursorKind kind = clang_getCursorKind(child);
    switch (kind) {
    case CXCursor_UnexposedExpr:
    case CXCursor_CallExpr:
        break;
    default:
        return ClangIndexer::nullCursor;
    }

    const CXCursor var = clang_getCursorReferenced(child);
    kind = clang_getCursorKind(var);
    switch (kind) {
    case CXCursor_ObjCIvarDecl:
    case CXCursor_VarDecl:
    case CXCursor_FieldDecl:
    case CXCursor_ParmDecl:
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
    case CXCursor_ConversionFunction:
        break;
    default:
        if (!clang_isInvalid(kind)) {
            error() << "Got unexpected cursor" << deleteStatement << var;
            // assert(0);
        }
        return ClangIndexer::nullCursor;
    }

    CXCursor ref = RTags::findChild(var, CXCursor_TypeRef);
    if (ref != CXCursor_TypeRef)
        ref = RTags::findChild(var, CXCursor_TemplateRef);
    kind = clang_getCursorKind(ref);
    switch (kind) {
    case CXCursor_TypeRef:
    case CXCursor_TemplateRef:
        break;
    default:
        return ClangIndexer::nullCursor;
    }

    const CXCursor referenced = clang_getCursorReferenced(ref);
    kind = clang_getCursorKind(referenced);
    switch (kind) {
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
        break;
    default:
        return ClangIndexer::nullCursor;
    }
    const CXCursor destructor = RTags::findChild(referenced, CXCursor_Destructor);
    return destructor;
}

template <typename T>
struct Updater
{
    Updater(T &v, const T &nv = T()) : var(v), newValue(nv) {}
    ~Updater() { var = newValue; }

    T &var;
    T newValue;
};

CXChildVisitResult ClangIndexer::indexVisitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
    ClangIndexer *indexer = static_cast<ClangIndexer*>(data);
    ++indexer->mCursorsVisited;
    // error() << "indexVisitor" << cursor;
    // FILE *f = fopen("/tmp/clangindex.log", "a");
    // String str;
    // Log(&str) << cursor;
    // fwrite(str.constData(), 1, str.size(), f);
    // fwrite("\n", 1, 1, f);
    // fclose(f);
    const Updater<CXCursor> lastCursorUpdater(indexer->mLastCursor, cursor);

    const CXCursorKind kind = clang_getCursorKind(cursor);
    const RTags::CursorType type = RTags::cursorType(kind);
    if (type == RTags::Type_Other) {
        return CXChildVisit_Recurse;
    }

    bool blocked = false;

    Location loc = indexer->createLocation(cursor, &blocked);

    if (blocked) {
        // error() << "blocked" << cursor;
        ++indexer->mBlocked;
        return CXChildVisit_Continue;
    } else if (loc.isNull()) {
        // error() << "Got null" << cursor;
        return CXChildVisit_Recurse;
    }
    for (const String &debug : indexer->mDebugLocations) {
        if (debug == "all" || debug == loc) {
            Log log(LogLevel::Error);
            log << cursor;
            CXCursor ref = clang_getCursorReferenced(cursor);
            if (!clang_isInvalid(clang_getCursorKind(ref)) && !clang_equalCursors(ref, cursor)) {
                log << "refs" << ref;
            }
            break;
        }
    }
    ++indexer->mAllowed;
    if (indexer->mLogFile) {
        String out;
        Log(&out) << cursor;
        fwrite(out.constData(), 1, out.size(), indexer->mLogFile);
        fwrite("\n", 1, 1, indexer->mLogFile);
    }

    if (testLog(LogLevel::VerboseDebug)) {
        Log log(LogLevel::VerboseDebug);
        log << cursor;
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (!clang_isInvalid(clang_getCursorKind(ref)) && !clang_equalCursors(ref, cursor)) {
            log << "refs" << ref;
        }
    }

    if (Symbol::isClass(kind)) {
        indexer->mLastClass = loc;
    } else {
        if (kind == CXCursor_CXXBaseSpecifier) {
            indexer->handleBaseClassSpecifier(cursor);
            return CXChildVisit_Recurse;
        }
    }

    switch (type) {
    case RTags::Type_Cursor:
        indexer->handleCursor(cursor, kind, loc);
        break;
    case RTags::Type_Include:
        indexer->handleInclude(cursor, kind, loc);
        break;
    case RTags::Type_Reference:
        switch (kind) {
        case CXCursor_OverloadedDeclRef: {
            const int count = clang_getNumOverloadedDecls(cursor);
            for (int i=0; i<count; ++i) {
                const CXCursor ref = clang_getOverloadedDecl(cursor, i);
                indexer->handleReference(cursor, kind, loc, ref, parent);
            }
            break; }
        case CXCursor_CXXDeleteExpr:
            indexer->handleReference(cursor, kind, loc, findDestructorForDelete(cursor), parent);
            break;
        case CXCursor_CallExpr: {
            // uglehack, see rtags/tests/nestedClassConstructorCallUgleHack/
            const CXCursor ref = clang_getCursorReferenced(cursor);
            if (clang_getCursorKind(ref) == CXCursor_Constructor
                && (clang_getCursorKind(indexer->mLastCursor) == CXCursor_TypeRef
                    || clang_getCursorKind(indexer->mLastCursor) == CXCursor_TemplateRef)
                && clang_getCursorKind(parent) != CXCursor_VarDecl) {
                loc = indexer->createLocation(indexer->mLastCursor);
                indexer->handleReference(indexer->mLastCursor, kind, loc, ref, parent);
            } else {
                indexer->handleReference(cursor, kind, loc, ref, parent);
            }
            break; }
        default:
            indexer->handleReference(cursor, kind, loc, clang_getCursorReferenced(cursor), parent);
            break;
        }
        break;
    case RTags::Type_Other:
        assert(0);
        break;
    }
    return CXChildVisit_Recurse;
}

static inline bool isImplicit(const CXCursor &cursor)
{
    return clang_equalLocations(clang_getCursorLocation(cursor),
                                clang_getCursorLocation(clang_getCursorSemanticParent(cursor)));
}

bool ClangIndexer::superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                            const Location &location, const CXCursor &/*ref*/,
                                                            const CXCursor &parent, Symbol **cursorPtr)
{
    // This is for references to superclass template functions. Awful awful
    // shit. See https://github.com/Andersbakken/rtags/issues/62 and commit
    // for details. I really should report this as a bug.
    if (cursorPtr)
        *cursorPtr = 0;
    if (kind != CXCursor_MemberRefExpr && clang_getCursorKind(parent) != CXCursor_CallExpr)
        return false;

    const CXCursor templateRef = RTags::findChild(cursor, CXCursor_TemplateRef);
    if (templateRef != CXCursor_TemplateRef)
        return false;

    const CXCursor classTemplate = clang_getCursorReferenced(templateRef);
    if (classTemplate != CXCursor_ClassTemplate)
        return false;
    FILE *f = fopen(location.path().constData(), "r");
    if (!f)
        return false;

    const CXSourceRange range = clang_getCursorExtent(cursor);
    const CXSourceLocation end = clang_getRangeEnd(range);
    unsigned int offset;
    clang_getSpellingLocation(end, 0, 0, 0, &offset);

    String name;
    while (offset > 0) {
        fseek(f, --offset, SEEK_SET);
        char ch = static_cast<char>(fgetc(f));
        if (isalnum(ch) || ch == '_' || ch == '~') {
            name.prepend(ch);
        } else {
            break;
        }
    }
    fclose(f);
    if (!name.isEmpty()) {
        RTags::Filter out;
        out.kinds.insert(CXCursor_MemberRefExpr);
        const int argCount = RTags::children(parent, RTags::Filter(), out).size();
        RTags::Filter in(RTags::Filter::And);
        in.names.insert(name);
        in.argumentCount = argCount;
        const List<CXCursor> alternatives = RTags::children(classTemplate, in);
        switch (alternatives.size()) {
        case 1:
            // ### not sure this is correct with line/col
            return handleReference(cursor, kind,
                                   Location(location.fileId(), location.line(), location.column() + 1),
                                   alternatives.first(), parent, cursorPtr);
            break;
        case 0:
            break;
        default:
            warning() << "Can't decide which of these symbols are right for me"
                      << cursor << alternatives
                      << "Need to parse types";
            break;
        }
    }
    return false;
}

bool ClangIndexer::handleReference(const CXCursor &cursor, CXCursorKind kind,
                                   const Location &location, CXCursor ref,
                                   const CXCursor &parent, Symbol **cursorPtr)
{
    if (cursorPtr)
        *cursorPtr = 0;
    // error() << "handleReference" << cursor << kind << location << ref;
    const CXCursorKind refKind = clang_getCursorKind(ref);
    if (clang_isInvalid(refKind)) {
        return superclassTemplateMemberFunctionUgleHack(cursor, kind, location, ref, parent, cursorPtr);
    }

    bool isOperator = false;
    if (kind == CXCursor_CallExpr && (refKind == CXCursor_CXXMethod
                                      || refKind == CXCursor_FunctionDecl
                                      || refKind == CXCursor_FunctionTemplate)) {
        // These are bullshit. for this construct:
        // foo.bar();
        // the position of the cursor is at the foo, not the bar.
        // They are not interesting for followLocation, renameSymbol or find
        // references so we toss them.
        // For functions it can be the position of the namespace.
        // E.g. Foo::bar(); cursor is on Foo
        // For constructors they happen to be the only thing we have that
        // actually refs the constructor and not the class so we have to keep
        // them for that.
        return false;
    }

    Location refLoc = createLocation(ref);
    if (!refLoc.isValid()) {
        // ### THIS IS NOT SOLVED
        // if (kind == CXCursor_ObjCMessageExpr) {
        //    mIndexDataMessage.mPendingReferenceMap[RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(ref)))].insert(location);
        //     // insert it, we'll hook up the target and references later
        //     return handleCursor(cursor, kind, location, cursorPtr);
        // }
        return false;
    }

    switch (refKind) {
    case CXCursor_Constructor:
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
    case CXCursor_Destructor:
    case CXCursor_FunctionTemplate: {
        while (true) {
            const CXCursor general = clang_getSpecializedCursorTemplate(ref);
            if (!clang_Cursor_isNull(general) && createLocation(general) == refLoc) {
                ref = general;
            } else {
                break;
            }
        }
        if (refKind == CXCursor_FunctionDecl)
            break;
        if (refKind == CXCursor_Constructor || refKind == CXCursor_Destructor) {
            if (isImplicit(ref)) {
                return false;
            }
        } else {
            CXStringScope scope = clang_getCursorDisplayName(ref);
            const char *data = scope.data();
            if (data) {
                const int len = strlen(data);
                if (len > 8 && !strncmp(data, "operator", 8) && !isalnum(data[8]) && data[8] != '_') {
                    if (isImplicit(ref)) {
                        return false; // eat implicit operator calls
                    }
                    isOperator = true;
                }
            }
        }
        break; }
    default:
        break;
    }

    const String refUsr = usr(ref);
    if (refUsr.isEmpty()) {
        return false;
    }

    FindResult result;
    auto reffedCursor = findSymbol(refLoc, &result);
    Map<String, uint16_t> &targets = unit(location)->targets[location];
    if (result == NotFound && !mUnionRecursion) {
        CXCursor parent = clang_getCursorSemanticParent(ref);
        CXCursor best = ClangIndexer::nullCursor;
        while (true) {
            const CXCursorKind kind = clang_getCursorKind(parent);
            if (kind != CXCursor_UnionDecl)
                break;
            best = parent;
            parent = clang_getCursorSemanticParent(parent);
        }
        if (best == CXCursor_UnionDecl) {
            mUnionRecursion = true;
            // for anonymous unions we don't get to their fields with normal
            // recursing of the AST. In these cases we visit the union decl
            clang_visitChildren(best, ClangIndexer::indexVisitor, this);
            mUnionRecursion = false;
            reffedCursor = findSymbol(refLoc, &result);
        }
    }
    uint16_t refTargetValue;
    if (result == Found) {
        refTargetValue = reffedCursor.targetsValue();
    } else {
        refTargetValue = RTags::createTargetsValue(refKind, clang_isCursorDefinition(ref));
    }

    assert(!refUsr.isEmpty());
    targets[refUsr] = refTargetValue;
    Symbol &c = unit(location)->symbols[location];
    if (cursorPtr)
        *cursorPtr = &c;

    // We need the new cursor to replace the symbolLength. This is important
    // in the following case:
    // struct R { R(const &r); ... }
    // R foo();
    // ...
    // R r = foo();

    // The first cursor on foo() will be a reference to the copy constructor and
    // this cursor will have a symbolLength of 1. Thus you won't be able to jump
    // to foo from the o. This is fixed by making sure the newer target, if
    // better, gets to decide on the symbolLength

    // The !isCursor is var decls and field decls where we set up a target even
    // if they're not considered references

    if (!c.isNull()) {
        if (RTags::isCursor(c.kind))
            return true;
        auto best = targets.end();
        int bestRank = -1;
        for (auto it = targets.begin(); it != targets.end(); ++it) {
            const int r = RTags::targetRank(RTags::targetsValueKind(it->second));
            if (r > bestRank || (r == bestRank && RTags::targetsValueIsDefinition(it->second))) {
                bestRank = r;
                best = it;
            }
        }
        if (best != targets.end() && best->first != refUsr) { // another target is better
            return true;
        }
    }

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 16)
    if (result == Found) {
        c.size = reffedCursor.size;
        c.alignment = reffedCursor.alignment;
    } else {
        const CXType type = clang_getCursorType(ref);
        if (type.kind != CXType_LValueReference && type.kind != CXType_RValueReference && type.kind != CXType_Unexposed) {
            c.size = clang_Type_getSizeOf(type);
            c.alignment = std::max<int16_t>(-1, clang_Type_getAlignOf(type));
        }
    }
#endif

    CXSourceRange range = clang_getCursorExtent(cursor);
    CXSourceLocation rangeStart = clang_getRangeStart(range);
    CXSourceLocation rangeEnd = clang_getRangeEnd(range);
    unsigned int startLine, startColumn, endLine, endColumn;
    clang_getSpellingLocation(rangeStart, 0, &startLine, &startColumn, 0);
    clang_getSpellingLocation(rangeEnd, 0, &endLine, &endColumn, 0);
    c.startLine = startLine;
    c.endLine = endLine;
    c.startColumn = startColumn;
    c.endColumn = endColumn;
    c.definition = false;
    c.kind = kind;
    c.location = location;
    c.symbolName = result == Found ? reffedCursor.symbolName : addNamePermutations(ref, refLoc, RTags::Type_Reference);

    if (isOperator) {
        unsigned int start, end;
        clang_getSpellingLocation(rangeStart, 0, 0, 0, &start);
        clang_getSpellingLocation(rangeEnd, 0, 0, 0, &end);
        c.symbolLength = end - start;
    } else {
        c.symbolLength = result == Found ? reffedCursor.symbolLength : symbolLength(refKind, ref);
    }
    if (!c.symbolLength) {
        unit(location)->symbols.remove(location);
        if (cursorPtr)
            *cursorPtr = 0;
        return false;
    }
    setType(c, clang_getCursorType(cursor));

    return true;
}


void ClangIndexer::addOverriddenCursors(const CXCursor &cursor, const Location &location)
{
    // error() << "addOverriddenCursors" << cursor << location;
    CXCursor *overridden;
    unsigned int count;
    clang_getOverriddenCursors(cursor, &overridden, &count);
    if (!overridden)
        return;
    for (unsigned int i=0; i<count; ++i) {
        // error() << location << "got" << i << count << loc;

        const String usr = ::usr(overridden[i]);
        assert(!usr.isEmpty());
        // assert(!locCursor.usr.isEmpty());

        // error() << location << "targets" << overridden[i];
        unit(location)->targets[location][usr] = 0;
        addOverriddenCursors(overridden[i], location);
    }
    clang_disposeOverriddenCursors(overridden);
}

void ClangIndexer::handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    assert(kind == CXCursor_InclusionDirective);
    (void)kind;
    CXFile includedFile = clang_getIncludedFile(cursor);
    if (includedFile) {
        const Location refLoc = createLocation(includedFile, 1, 1);
        if (!refLoc.isNull()) {
            Symbol &c = unit(location)->symbols[location];
            if (!c.isNull())
                return;

            String include = "#include ";
            const Path path = refLoc.path();
            assert(mSource.fileId);
            unit(location)->symbolNames[(include + path)].insert(location);
            unit(location)->symbolNames[(include + path.fileName())].insert(location);
            mIndexDataMessage.includes().push_back(std::make_pair(location.fileId(), refLoc.fileId()));
            c.symbolName = "#include " + RTags::eatString(clang_getCursorDisplayName(cursor));
            c.kind = cursor.kind;
            c.symbolLength = c.symbolName.size() + 2;
            c.location = location;
            unit(location)->targets[location][refLoc.path()] = 0; // ### what targets value to create for this?
            // this fails for things like:
            // # include    <foobar.h>
            return;
        }
    }
    error() << "couldn't create included file" << cursor;
}


void ClangIndexer::handleBaseClassSpecifier(const CXCursor &cursor)
{
    auto &lastClass = unit(mLastClass)->symbols[mLastClass];
    if (!lastClass.isClass()) {
        // this happens with some weird macros in /usr/include/dispatch/io.h:161:1 and others on Mac
        // error() << "Couldn't find class for" << cursor << mLastClass;
        return;
    }
    const CXCursor ref = clang_getCursorReferenced(cursor);
    if (clang_isInvalid(clang_getCursorKind(ref))) // this happens when the base class is a template parameter
        return;

    assert(lastClass.isClass());
    const String usr = ::usr(ref);
    if (usr.isEmpty()) {
        error() << "Couldn't find usr for" << clang_getCursorReferenced(cursor) << cursor << mLastClass;
        return;
    }
    assert(!usr.isEmpty());
    lastClass.baseClasses << usr;
}

void ClangIndexer::addArguments(Symbol *sym, const CXCursor &cursor)
{
    const int count = clang_Cursor_getNumArguments(cursor);
    if (count > 0) {
        sym->arguments.resize(count);
        for (int i=0; i<count; ++i) {
            sym->arguments[i] = createLocation(clang_Cursor_getArgument(cursor, i));
        }
    }
}

bool ClangIndexer::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location, Symbol **cursorPtr)
{
    const String usr = ::usr(cursor);
    // error() << "Got a cursor" << cursor;
    Symbol &c = unit(location)->symbols[location];
    if (cursorPtr)
        *cursorPtr = &c;
    if (!c.isNull()) {
        if (c.kind == CXCursor_MacroExpansion) {
            addNamePermutations(cursor, location, RTags::Type_Cursor);
            unit(location)->usrs[usr].insert(location);
        }
        return true;
    }

    // if (mLogFile) {
    //     String out;
    //     Log(&out) << cursor << a;
    //     fwrite(out.constData(), 1, out.size(), mLogFile);
    //     fwrite("\n", 1, 1, mLogFile);
    // }
    CXStringScope name = clang_getCursorSpelling(cursor);
    const char *cstr = name.data();
    c.symbolLength = cstr ? strlen(cstr) : 0;
    const CXType type = clang_getCursorType(cursor);
    setType(c, type);
    c.location = location;
    c.usr = usr;
    if (!c.symbolLength) {
        // this is for these constructs:
        // typedef struct {
        //    int a;
        // } foobar;
        //
        // We end up not getting a spelling for the cursor

        switch (kind) {
        case CXCursor_ClassDecl:
            c.symbolLength = 5;
            c.symbolName = "class";
            break;
        case CXCursor_UnionDecl:
            c.symbolLength = 5;
            c.symbolName = "union";
            break;
        case CXCursor_StructDecl:
            c.symbolLength = 6;
            c.symbolName = "struct";
            break;
        default:
            unit(location)->symbols.remove(location);
            if (cursorPtr)
                *cursorPtr = 0;
            return false;
        }
    } else {
        if (kind == CXCursor_VarDecl) {
            std::shared_ptr<RTags::Auto> resolvedAuto = RTags::resolveAuto(cursor);
            if (resolvedAuto) {
                c.flags |= Symbol::Auto;
                if (resolvedAuto->type.kind != CXType_Invalid) {
                    setType(c, resolvedAuto->type);
                    if (!clang_equalCursors(resolvedAuto->cursor, nullCursor)) {
                        const Location loc = createLocation(clang_getCursorLocation(mLastCursor));
                        Symbol *cursorPtr = 0;
                        if (loc.fileId() && handleReference(mLastCursor, CXCursor_TypeRef, loc,
                                                            resolvedAuto->cursor, nullCursor, &cursorPtr)) {
                            cursorPtr->symbolLength = 4;
                            cursorPtr->type = c.type;
                            cursorPtr->endLine = c.startLine;
                            cursorPtr->endColumn = c.startColumn + 4;
                            cursorPtr->flags |= Symbol::AutoRef;
                        }
                    }
                } else {
                    warning() << "Couldn't resolve auto for" << cursor;
                }
            }
        }

        c.symbolName = addNamePermutations(cursor, location, RTags::Type_Cursor);
    }

    const CXSourceRange range = clang_getCursorExtent(cursor);
    const CXSourceLocation rangeStart = clang_getRangeStart(range);
    const CXSourceLocation rangeEnd = clang_getRangeEnd(range);
    unsigned int startLine, startColumn, endLine, endColumn;
    clang_getSpellingLocation(rangeStart, 0, &startLine, &startColumn, 0);
    clang_getSpellingLocation(rangeEnd, 0, &endLine, &endColumn, 0);
    c.startLine = startLine;
    c.endLine = endLine;
    c.startColumn = startColumn;
    c.endColumn = endColumn;

    switch (kind) {
    case CXCursor_EnumConstantDecl:

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 2)
        c.enumValue = clang_getEnumConstantDeclValue(cursor);
#else
        c.definition = 1;
#endif
        break;
    case CXCursor_FieldDecl:
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 30)
        c.fieldOffset = std::max<int16_t>(-1, clang_Cursor_getOffsetOfField(cursor));
#endif
        // fall through
    default:
        c.definition = clang_isCursorDefinition(cursor);
        break;
    }

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 16)
    if (!(c.flags & (Symbol::Auto|Symbol::AutoRef))
        && c.type != CXType_LValueReference
        && c.type != CXType_RValueReference
        && c.type != CXType_Unexposed) {
        c.size = clang_Type_getSizeOf(type);
        c.alignment = std::max<int16_t>(-1, clang_Type_getAlignOf(type));
    }
#endif

    c.kind = kind;
    c.linkage = clang_getCursorLinkage(cursor);
    // apparently some function decls will give a different usr for
    // their definition and their declaration.  Using the canonical
    // cursor's usr allows us to join them. Check JSClassRelease in
    // JavaScriptCore for an example.
    unit(location)->usrs[c.usr].insert(location);
    if (c.linkage == CXLinkage_External && !c.isDefinition()) {
        switch (c.kind) {
        case CXCursor_FunctionDecl:
        case CXCursor_VarDecl: {
            const auto kind = clang_getCursorKind(clang_getCursorSemanticParent(cursor));
            switch (kind) {
            case CXCursor_ClassDecl:
            case CXCursor_ClassTemplate:
            case CXCursor_StructDecl:
                break;
            default:
                unit(location)->targets[location][usr] = RTags::createTargetsValue(kind, true);
                break;
            }
            break; }
        default:
            break;
        }
    }

    if (!(ClangIndexer::serverOpts() & Server::NoComments)) {
        const CXComment comment = clang_Cursor_getParsedComment(cursor);
        if (clang_Comment_getKind(comment) != CXComment_Null) {
            c.briefComment = RTags::eatString(clang_Cursor_getBriefCommentText(cursor));
            c.xmlComment = RTags::eatString(clang_FullComment_getAsXML(comment));
        }
    }

    switch (c.kind) {
    case CXCursor_CXXMethod:
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
        if (clang_CXXMethod_isPureVirtual(cursor))
            c.flags |= Symbol::PureVirtualMethod;
        else
#endif
        if (clang_CXXMethod_isVirtual(cursor))
            c.flags |= Symbol::VirtualMethod;

        if (clang_CXXMethod_isStatic(cursor))
            c.flags |= Symbol::StaticMethod;
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 27)
        if (clang_CXXMethod_isConst(cursor))
            c.flags |= Symbol::ConstMethod;
#endif

        addOverriddenCursors(cursor, location);
        // fall through
    case CXCursor_FunctionDecl:
    case CXCursor_FunctionTemplate:
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 19)
        if (clang_Cursor_isVariadic(cursor))
            c.flags |= Symbol::Variadic;
#endif
        addArguments(&c, cursor);
        break;
    case CXCursor_Constructor:
        addArguments(&c, cursor);
        // fall through
    case CXCursor_Destructor:
        // these are for joining constructors/destructor with their classes (for renaming symbols)
        assert(!::usr(clang_getCursorSemanticParent(cursor)).isEmpty());
        unit(location)->targets[location][::usr(clang_getCursorSemanticParent(cursor))] = 0;
        break;
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate: {
        const CXCursor specialization = clang_getSpecializedCursorTemplate(cursor);
        if (!(clang_equalCursors(specialization, nullCursor))) {
            unit(location)->targets[location][::usr(specialization)] = 0;
            c.flags |= Symbol::TemplateSpecialization;
        }
        break; }
    default:
        break;
    }

    return true;
}

bool ClangIndexer::parse()
{
    StopWatch sw;
    assert(!mClangUnit);
    assert(!mIndex);
    mIndex = clang_createIndex(0, 1);
    assert(mIndex);
    Flags<Source::CommandLineFlag> commandLineFlags = Source::Default;
    if (ClangIndexer::serverOpts() & Server::PCHEnabled)
        commandLineFlags |= Source::PCHEnabled;

    Flags<CXTranslationUnit_Flags> flags = CXTranslationUnit_DetailedPreprocessingRecord;
    bool pch;
    switch (mSource.language) {
    case Source::CPlusPlus11Header:
    case Source::CPlusPlusHeader:
    case Source::CHeader:
        flags |= CXTranslationUnit_Incomplete;
        pch = true;
        break;
    default:
        pch = false;
        break;
    }

    List<CXUnsavedFile> unsavedFiles(mUnsavedFiles.size() + 1);
    int unsavedIndex = 0;
    for (const auto &it : mUnsavedFiles) {
        unsavedFiles[unsavedIndex++] = {
            it.first.constData(),
            it.second.constData(),
            static_cast<unsigned long>(it.second.size())
        };
    }

    if (testLog(LogLevel::Debug))
        debug() << "CI::parse: " << mSource.toCommandLine(commandLineFlags) << "\n";

    // for (const auto it : mSource.toCommandLine(commandLineFlags)) {
    //     error("[%s]", it.constData());
    // }
    bool usedPch = false;
    const List<String> args = mSource.toCommandLine(commandLineFlags, &usedPch);
    if (usedPch)
        mIndexDataMessage.setFlag(IndexDataMessage::UsedPCH);

    RTags::parseTranslationUnit(mSourceFile, args, mClangUnit,
                                mIndex, &unsavedFiles[0], unsavedIndex, flags, &mClangLine);

    warning() << "CI::parse loading unit:" << mClangLine << " " << (mClangUnit != 0);
    if (mClangUnit) {
        if (pch && ClangIndexer::serverOpts() & Server::PCHEnabled) {
            Path path = RTags::encodeSourceFilePath(mDataDir, mProject, mSource.fileId);
            Path::mkdir(path, Path::Recursive);
            path << "pch.h.gch";
            Path tmp = path;
            tmp << ".tmp";
            clang_saveTranslationUnit(mClangUnit, tmp.constData(), clang_defaultSaveOptions(mClangUnit));
            rename(tmp.constData(), path.constData());
            warning() << "SAVED PCH" << path;
        }
        mParseDuration = sw.elapsed();
        return true;
    }
    error() << "Failed to parse" << mClangLine;
    mIndexDataMessage.setFlag(IndexDataMessage::ParseFailure);
    return false;
}

static inline Map<String, Set<Location> > convertTargets(const Map<Location, Map<String, uint16_t> > &in)
{
    Map<String, Set<Location> > ret;
    for (const auto &v : in) {
        for (const auto &u : v.second) {
            ret[u.first].insert(v.first);
        }
    }
    return ret;
}

bool ClangIndexer::writeFiles(const Path &root, String &error)
{
    for (const auto &unit : mUnits) {
        if (!(mIndexDataMessage.files().value(unit.first) & IndexDataMessage::Visited)) {
            ::error() << "Wanting to write something for"
                      << unit.first << Location::path(unit.first)
                      << "but we didn't visit it" << mSource.sourceFile()
                      << unit.second->targets.size()
                      << unit.second->usrs.size()
                      << unit.second->symbolNames.size()
                      << unit.second->symbols.size();
            continue;
        }
        assert(mIndexDataMessage.files().value(unit.first) & IndexDataMessage::Visited);
        String unitRoot = root;
        unitRoot << unit.first;
        Path::mkdir(unitRoot, Path::Recursive);
        // ::error() << "Writing file" << Location::path(unit.first) << unitRoot << unit.second->symbols.size()
        //           << unit.second->targets.size()
        //           << unit.second->usrs.size()
        //           << unit.second->symbolNames.size();
        uint32_t fileMapOpts = 0;
        if (ClangIndexer::serverOpts() & Server::NoFileLock)
            fileMapOpts |= FileMap<int, int>::NoLock;
        if (!FileMap<Location, Symbol>::write(unitRoot + "/symbols", unit.second->symbols, fileMapOpts)) {
            error = "Failed to write symbols";
            return false;
        }
        if (!FileMap<String, Set<Location> >::write(unitRoot + "/targets", convertTargets(unit.second->targets), fileMapOpts)) {
            error = "Failed to write targets";
            return false;
        }
        if (!FileMap<String, Set<Location> >::write(unitRoot + "/usrs", unit.second->usrs, fileMapOpts)) {
            error = "Failed to write usrs";
            return false;
        }
        if (!FileMap<String, Set<Location> >::write(unitRoot + "/symnames", unit.second->symbolNames, fileMapOpts)) {
            error = "Failed to write symbolNames";
            return false;
        }
    }
    String sourceRoot = root;
    sourceRoot << mSource.fileId;
    Path::mkdir(sourceRoot, Path::Recursive);
    sourceRoot << "/info";
    FILE *f = fopen(sourceRoot.constData(), "w");
    if (!f) {
        return false;
    }

    fprintf(f, "%s\n%s\n",
            mSourceFile.constData(),
            String::join(mSource.toCommandLine(Source::Default|Source::IncludeCompiler|Source::IncludeSourceFile), " ").constData());
    fclose(f);

    return true;
}

static inline bool compareFile(CXFile l, CXFile r)
{
    CXString fnl = clang_getFileName(l);
    CXString fnr = clang_getFileName(r);
    const char *cstrl = clang_getCString(fnl);
    const char *cstrr = clang_getCString(fnr);
    bool ret = false;
    if (cstrl && cstrr && !strcmp(cstrl, cstrr)) {
        ret = true;
    }

    clang_disposeString(fnl);
    clang_disposeString(fnr);
    return ret;
}

static inline Diagnostic::Type convertDiagnosticType(CXDiagnosticSeverity sev)
{
    Diagnostic::Type type = Diagnostic::None;
    switch (sev) {
    case CXDiagnostic_Warning:
        type = Diagnostic::Warning;
        break;
    case CXDiagnostic_Error:
    case CXDiagnostic_Fatal:
        type = Diagnostic::Error;
        break;
    case CXDiagnostic_Note:
        type = Diagnostic::Note;
    default:
        break;
    }
    return type;
}

bool ClangIndexer::diagnose()
{
    if (!mClangUnit) {
        return false;
    }

    std::function<void(CXDiagnostic, Diagnostics &, bool)> process = [&](CXDiagnostic d, Diagnostics &m, bool displayCategory) {
        const Diagnostic::Type type = convertDiagnosticType(clang_getDiagnosticSeverity(d));
        if (type != Diagnostic::None) {
            const CXSourceLocation diagLoc = clang_getDiagnosticLocation(d);
            const uint32_t fileId = createLocation(diagLoc, 0).fileId();

            Location location;
            int length = -1;
            Map<Location, int> ranges;
            String message;
            if (displayCategory)
                message << RTags::eatString(clang_getDiagnosticCategoryText(d));
            if (!message.isEmpty())
                message << ": ";
            message << RTags::eatString(clang_getDiagnosticSpelling(d));

            const String option = RTags::eatString(clang_getDiagnosticOption(d, 0));
            if (!option.isEmpty()) {
                message << ": " << option;
            }

            const unsigned int rangeCount = clang_getDiagnosticNumRanges(d);
            bool ok = false;
            for (unsigned int rangePos = 0; rangePos < rangeCount; ++rangePos) {
                const CXSourceRange range = clang_getDiagnosticRange(d, rangePos);
                const CXSourceLocation start = clang_getRangeStart(range);
                const CXSourceLocation end = clang_getRangeEnd(range);

                unsigned int startOffset, endOffset;
                clang_getSpellingLocation(start, 0, 0, 0, &startOffset);
                clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                if (startOffset && endOffset) {
                    unsigned int line, column;
                    clang_getSpellingLocation(start, 0, &line, &column, 0);
                    const Location l(fileId, line, column);
                    if (!ok) {
                        ok = true;
                        location = l;
                        length = endOffset - startOffset;
                    } else {
                        ranges[l] = endOffset - startOffset;
                    }
                    ok = true;
                }
            }
            if (!ok) {
                unsigned int line, column;
                clang_getSpellingLocation(diagLoc, 0, &line, &column, 0);
                location = Location(fileId, line, column);
            }

            Diagnostic &diagnostic = m[location];
            diagnostic.type = type;
            diagnostic.message = std::move(message);
            diagnostic.ranges = std::move(ranges);
            diagnostic.length = length;

            if (CXDiagnosticSet children = clang_getChildDiagnostics(d)) {
                const unsigned int childCount = clang_getNumDiagnosticsInSet(children);
                for (unsigned i=0; i<childCount; ++i) {
                    process(clang_getDiagnosticInSet(children, i), diagnostic.children, false);
                }
                clang_disposeDiagnosticSet(children);
            }
        }
    };

    List<String> compilationErrors;
    const unsigned int diagnosticCount = clang_getNumDiagnostics(mClangUnit);

    for (unsigned int i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnostic(mClangUnit, i);
        const CXSourceLocation diagLoc = clang_getDiagnosticLocation(diagnostic);
        const uint32_t fileId = createLocation(diagLoc, 0).fileId();
        const CXDiagnosticSeverity sev = clang_getDiagnosticSeverity(diagnostic);
        // error() << "Got a dude" << clang_getCursor(mClangUnit, diagLoc) << fileId << mSource.fileId
        //         << sev << CXDiagnostic_Error;
        const CXCursor cursor = clang_getCursor(mClangUnit, diagLoc);
        const bool inclusionError = clang_getCursorKind(cursor) == CXCursor_InclusionDirective;
        if (inclusionError)
            mIndexDataMessage.setFlag(IndexDataMessage::InclusionError);
        Flags<IndexDataMessage::FileFlag> &flags = mIndexDataMessage.files()[fileId];
        if (fileId != mSource.fileId && !inclusionError && sev >= CXDiagnostic_Error && !(flags & IndexDataMessage::HeaderError)) {
            // We don't treat inclusions or code inside a macro expansion as a
            // header error
            CXFile expFile, spellingFile;
            unsigned expLine, expColumn, spellingLine, spellingColumn;
            clang_getExpansionLocation(diagLoc, &expFile, &expLine, &expColumn, 0);
            clang_getSpellingLocation(diagLoc, &spellingFile, &spellingLine, &spellingColumn, 0);
            if (expLine == spellingLine && expColumn == spellingColumn && compareFile(expFile, spellingFile))
                flags |= IndexDataMessage::HeaderError;
        }
        if (flags & IndexDataMessage::Visited) {
            process(diagnostic, mIndexDataMessage.diagnostics(), true);
        }
        // logDirect(RTags::DiagnosticsLevel, message.constData());

        const unsigned int fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
        for (unsigned int f=0; f<fixItCount; ++f) {
            CXSourceRange range;
            const CXStringScope stringScope = clang_getDiagnosticFixIt(diagnostic, f, &range);
            CXSourceLocation start = clang_getRangeStart(range);

            unsigned int line, column;
            CXFile file;
            clang_getSpellingLocation(start, &file, &line, &column, 0);
            if (!file)
                continue;
            CXStringScope fileName(clang_getFileName(file));

            const Location loc = createLocation(clang_getCString(fileName), line, column);
            if (mIndexDataMessage.files().value(loc.fileId()) & IndexDataMessage::Visited) {
                unsigned int startOffset, endOffset;
                CXSourceLocation end = clang_getRangeEnd(range);
                clang_getSpellingLocation(start, 0, 0, 0, &startOffset);
                clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                const char *string = clang_getCString(stringScope);
                assert(string);
                if (!*string) {
                    error("Fixit for %s Remove %d character%s",
                          loc.toString().constData(), endOffset - startOffset,
                          endOffset - startOffset > 1 ? "s" : "");
                } else if (endOffset == startOffset) {
                    error("Fixit for %s Insert \"%s\"",
                          loc.toString().constData(), string);
                } else {
                    error("Fixit for %s Replace %d character%s with \"%s\"",
                          loc.toString().constData(), endOffset - startOffset,
                          endOffset - startOffset > 1 ? "s" : "", string);
                }
                Diagnostic &entry = mIndexDataMessage.diagnostics()[Location(loc.fileId(), line, column)];
                entry.type = Diagnostic::Fixit;
                if (entry.message.isEmpty()) {
                    entry.message = String::format<64>("did you mean '%s'?", string);
                }
                entry.length = endOffset - startOffset;
                mIndexDataMessage.fixIts()[loc.fileId()].insert(FixIt(line, column, endOffset - startOffset, string));
            }
        }
        clang_disposeDiagnostic(diagnostic);
    }

    for (const auto &it : mIndexDataMessage.files()) {
        if (it.second & IndexDataMessage::Visited) {
            const Location loc(it.first, 0, 0);
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 21)
            CXFile file = clang_getFile(mClangUnit, loc.path().constData());
            if (file) {
                if (CXSourceRangeList *skipped = clang_getSkippedRanges(mClangUnit, file)) {
                    const unsigned int count = skipped->count;
                    for (unsigned int i=0; i<count; ++i) {
                        CXSourceLocation start = clang_getRangeStart(skipped->ranges[i]);

                        unsigned int line, column, startOffset, endOffset;
                        clang_getSpellingLocation(start, 0, &line, &column, &startOffset);
                        Diagnostic &entry = mIndexDataMessage.diagnostics()[Location(loc.fileId(), line, column)];
                        if (entry.type == Diagnostic::None) {
                            CXSourceLocation end = clang_getRangeEnd(skipped->ranges[i]);
                            clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                            entry.type = Diagnostic::Skipped;
                            entry.length = endOffset - startOffset;
                            // error() << line << column << startOffset << endOffset;
                        }
                    }

                    clang_disposeSourceRangeList(skipped);
                    if (count)
                        continue;
                }
            }
#endif
            const Map<Location, Diagnostic>::const_iterator x = mIndexDataMessage.diagnostics().lower_bound(loc);
            if (x == mIndexDataMessage.diagnostics().end() || x->first.fileId() != it.first) {
                mIndexDataMessage.diagnostics()[loc] = Diagnostic();
            }
        }
    }
    return true;
}

bool ClangIndexer::visit()
{
    if (!mClangUnit || !mSource.fileId) {
        return false;
    }

    StopWatch watch;

    clang_visitChildren(clang_getTranslationUnitCursor(mClangUnit),
                        ClangIndexer::indexVisitor, this);

    for (const auto &it : mIndexDataMessage.files()) {
        if (it.second & IndexDataMessage::Visited)
            addFileSymbol(it.first);
    }

    mVisitDuration = watch.elapsed();

    if (testLog(LogLevel::VerboseDebug)) {
        VerboseVisitorUserData u = { 0, "<VerboseVisitor " + mClangLine + ">\n", this };
        clang_visitChildren(clang_getTranslationUnitCursor(mClangUnit),
                            ClangIndexer::verboseVisitor, &u);
        u.out += "</VerboseVisitor " + mClangLine + ">";
        if (getenv("RTAGS_INDEXERJOB_DUMP_TO_FILE")) {
            char buf[1024];
            snprintf(buf, sizeof(buf), "/tmp/%s.log", Location::path(mSource.fileId).fileName());
            FILE *f = fopen(buf, "w");
            assert(f);
            fwrite(u.out.constData(), 1, u.out.size(), f);
            fclose(f);
        } else {
            logDirect(LogLevel::VerboseDebug, u.out);
        }
    }
    return true;
}

CXChildVisitResult ClangIndexer::verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
    Location loc = u->indexer->createLocation(cursor);
    if (loc.fileId()) {
        CXCursor ref = clang_getCursorReferenced(cursor);

        VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
        if (u->indent >= 0)
            u->out += String(u->indent, ' ');
        u->out += RTags::cursorToString(cursor);
        if (clang_equalCursors(ref, cursor)) {
            u->out += " refs self";
        } else if (!clang_equalCursors(ref, nullCursor)) {
            u->out += " refs " + RTags::cursorToString(ref);
        }

        if (loc.fileId() && u->indexer->mIndexDataMessage.files().value(loc.fileId()) & IndexDataMessage::Visited) {
            if (u->indexer->unit(loc)->symbols.contains(loc)) {
                u->out += " used as cursor\n";
            } else {
                u->out += " not used\n";
            }
        } else {
            u->out += " not indexed\n";
        }
    }
    if (u->indent >= 0) {
        u->indent += 2;
        clang_visitChildren(cursor, ClangIndexer::verboseVisitor, userData);
        u->indent -= 2;
        return CXChildVisit_Continue;
    } else {
        return CXChildVisit_Recurse;
    }
}

void ClangIndexer::addFileSymbol(uint32_t file)
{
    const Location loc(file, 1, 1);
    const Path path = Location::path(file);
    auto ref = unit(loc);
    ref->symbolNames[path].insert(loc);
    const char *fn = path.fileName();
    ref->symbolNames[fn].insert(loc);
    Symbol &sym = ref->symbols[loc];
    sym.location = loc;
}

int ClangIndexer::symbolLength(CXCursorKind kind, const CXCursor &cursor)
{
    if (kind == CXCursor_VarDecl) {
        if (RTags::resolveAuto(cursor))
            return 4;
    }

    CXStringScope name = clang_getCursorSpelling(cursor);
    const char *cstr = name.data();
    if (cstr)
        return strlen(cstr);

    // this is for these constructs:
    //         ||
    //         \/
    // typedef struct {
    //    int a;
    // } foobar;
    //
    // We end up not getting a spelling for the cursor

    switch (kind) {
    case CXCursor_ClassDecl:
    case CXCursor_UnionDecl:
        return 5;
    case CXCursor_StructDecl:
        return 6;
    default:
        break;
    }
    return 0;
}

Symbol ClangIndexer::findSymbol(const Location &location, FindResult *result) const
{
    auto it = mUnits.find(location.fileId());
    if (it != mUnits.end()) {
        bool ok;
        Symbol ret = it->second->symbols.value(location, Symbol(), &ok);
        if (ok) {
            *result = Found;
            return ret;
        }
    }

    if (mIndexDataMessage.files().value(location.fileId()) & IndexDataMessage::Visited) {
        *result = NotFound;
    } else {
        *result = NotIndexed;
    }
    return Symbol();
}
