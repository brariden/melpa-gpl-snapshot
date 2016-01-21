{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- The JSON api that we expose
--
-- Some general design principles:
--
-- * We have a single Request type for requests sent by the editor to the client
--   and a single Response type for responses sent back from the client to the
--   editor.
--
-- * We try to roughly have a one-to-one mapping between the functions exported
--   by IdeSession; for instance, ide-backend's `updateSourceFileFromFile`
--   corresponds to the `RequestUpdateSourceFileFromFile` request and the
--   `ResponseUpdateSourceFileFromFile` response.
--
-- Hopefully with these principles in place the API should be predictable,
-- stable, and well documented.

module Stack.Ide.JsonAPI (
    -- * Requests
    Request(..)
  , RequestSessionUpdate(..)
  , Response(..)
  , ResponseSpanInfo(..)
  , ResponseExpType(..)
  , ResponseAnnExpType(..)
  , Ann(..)
  , CodeAnn(..)
  , VersionInfo(..)
  , Identifier
  , Targets(..)
  , Sequenced(..), unsequenced, withSameSeqAs
  , ByteString64(..)
  , sliceSpans
  ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import IdeSession.Types.Public hiding (idProp, Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Messages sent from the editor to the client
data Request =
  -- Update
    RequestUpdateSession [RequestSessionUpdate]
  -- Query
  | RequestGetSourceErrors
  | RequestGetLoadedModules
  | RequestGetSpanInfo SourceSpan
  | RequestGetExpTypes SourceSpan
  | RequestGetAnnExpTypes SourceSpan
  | RequestGetAutocompletion FilePath String
  -- Run
  | RequestRun Bool ModuleName Identifier
  | RequestProcessInput String
  | RequestProcessInterrupt
  | RequestProcessKill
  -- Misc
  | RequestShutdownSession
  deriving (Show, Eq)

-- | Session updates
data RequestSessionUpdate
  = RequestSessionUpdate -- Simply calls updateSession with no changes to trigger a recompile.
  | RequestUpdateTargets Targets
  | RequestUpdateSourceFile FilePath ByteString64
  | RequestUpdateSourceFileDelete FilePath
  | RequestUpdateDataFile FilePath ByteString64
  | RequestUpdateDataFileDelete FilePath
  | RequestUpdateGhcOpts [String]
  | RequestUpdateRtsOpts [String]
  | RequestUpdateRelativeIncludes [FilePath]
  | RequestUpdateCodeGeneration Bool
  | RequestUpdateEnv [(String, Maybe String)]
  | RequestUpdateArgs [String]
  deriving (Show, Eq)

-- TODO:
-- RequestUpdateStdoutBufferMode
-- RequestUpdateStderrBufferMode

-- | Messages sent back from the client to the editor
data Response =
    -- | Sent on session initialization
    ResponseWelcome VersionInfo
    -- | Nothing indicates the update completed
  | ResponseUpdateSession UpdateStatus
  | ResponseGetSourceErrors [SourceError]
  | ResponseGetLoadedModules [ModuleName]
  | ResponseGetSpanInfo [ResponseSpanInfo]
  | ResponseGetExpTypes [ResponseExpType]
  | ResponseGetAnnExpTypes [ResponseAnnExpType]
  | ResponseGetAutocompletion [IdInfo]
  -- Run
  | ResponseProcessOutput String
  | ResponseProcessDone RunResult
  | ResponseNoProcessError
  -- Misc
  | ResponseLog Text
  | ResponseInvalidRequest String
  | ResponseFatalError String
  | ResponseShutdownSession
  deriving (Show, Eq)

data ResponseSpanInfo =
    ResponseSpanInfo SpanInfo SourceSpan
  deriving (Show, Eq)

data ResponseExpType =
    ResponseExpType Text SourceSpan
  deriving (Show, Eq)

data ResponseAnnExpType =
    ResponseAnnExpType (Ann CodeAnn) SourceSpan
  deriving (Show, Eq)

data Ann a =
    Ann a (Ann a)
  | AnnGroup [Ann a]
  | AnnLeaf Text
  deriving (Show, Eq, Functor)

data CodeAnn =
    CodeIdInfo IdInfo
  deriving (Show, Eq)

-- | Client version
--
-- Standard versioning applies (major, minor, patch)
data VersionInfo =
    VersionInfo Int Int Int
  deriving (Show, Eq)

type Identifier = Text

-- | An extension of messages with an optional sequence code,
--   an uninterpreted JSON value. See (#39) for motivation.
data Sequenced a =
    NoSeq  a
  | HasSeq Value a
  deriving (Show, Eq, Functor)

unsequenced :: Sequenced a -> a
unsequenced (NoSeq    a) = a
unsequenced (HasSeq _ a) = a

withSameSeqAs :: Sequenced a -> b -> Sequenced b
withSameSeqAs sa b = b <$ sa

instance ToJSON a => ToJSON (Sequenced a) where
  toJSON (NoSeq    a) = toJSON a
  toJSON (HasSeq s a) =
    case toJSON a of
      Object o -> Object (H.insert "seq" s o)
      json_a   -> json_a

instance FromJSON a => FromJSON (Sequenced a) where
  parseJSON x
    = case x of
        Object o | Just s <- H.lookup "seq" o ->
          HasSeq s <$> parseJSON (Object $ H.delete "seq" o)
        _ ->
          NoSeq <$> parseJSON x

newtype ByteString64 = ByteString64 { unByteString64 :: ByteString }
    deriving (Eq, Read, Show, Ord)
instance ToJSON ByteString64 where
    toJSON (ByteString64 bs) = toJSON (Text.decodeUtf8 $ B64.encode bs)
instance FromJSON ByteString64 where
    parseJSON o =
        parseJSON o >>= either fail (return . ByteString64) . B64.decode . Text.encodeUtf8

--------------------------------------------------------------------------------
-- Misc utilities

-- | Useful for slicing up annotated text. Exported for client-side and
-- server-side code.
sliceSpans :: Int -> Text -> [(Int, Int, a)] -> [(Text, Maybe a)]
sliceSpans _ txt _ | Text.null txt = []
sliceSpans _ txt [] = [(txt, Nothing)]
sliceSpans ix txt ((fr, to, x) : xs) =
    appendNonNull before Nothing $
    appendNonNull chunk (Just x) $
    sliceSpans to rest' xs
  where
    appendNonNull t mx = if Text.null t then id else ((t, mx) :)
    (chunk, rest') = Text.splitAt ((to - ix) - fr') rest
    (before, rest) = Text.splitAt fr' txt
    fr' = max 0 (fr - ix)

--------------------------------------------------------------------------------
-- For the moment we use Aeson's built-in instance deriver

$(concat <$> mapM (deriveJSON defaultOptions)
  [ ''Ann
  , ''CodeAnn
  , ''Request
  , ''RequestSessionUpdate
  , ''Response
  , ''ResponseAnnExpType
  , ''ResponseExpType
  , ''ResponseSpanInfo
  , ''Targets
  , ''VersionInfo
  ])
