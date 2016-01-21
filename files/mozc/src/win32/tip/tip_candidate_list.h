// Copyright 2010-2016, Google Inc.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef MOZC_WIN32_TIP_TIP_CANDIDATE_LIST_H_
#define MOZC_WIN32_TIP_TIP_CANDIDATE_LIST_H_

#include <Unknwn.h>

#include <string>
#include <vector>

#include "base/port.h"

struct ITfCandidateList;

namespace mozc {
namespace commands {
class Output;
}  // commands

namespace win32 {
namespace tsf {

class TipCandidateListCallback {
 public:
  virtual ~TipCandidateListCallback();
  virtual void OnFinalize(size_t index, const wstring &candidate) = 0;
};

class TipCandidateList {
 public:
  // Returns an object that implements ITfFnSearchCandidateProvider.
  // |callback| will be called back when ITfCandidateList::SetResult
  // is called with CAND_FINALIZED. TipCandidateList will take the
  // ownership of |callback|. |callback| can be nullptr.
  // Caller must maintain the reference count of the returned object.
  static ITfCandidateList *New(const vector<wstring> &candidates,
                               TipCandidateListCallback *callback);
  static const IID& GetIID();

 private:
  DISALLOW_IMPLICIT_CONSTRUCTORS(TipCandidateList);
};

}  // namespace tsf
}  // namespace win32
}  // namespace mozc

#endif  // MOZC_WIN32_TIP_TIP_CANDIDATE_LIST_H_