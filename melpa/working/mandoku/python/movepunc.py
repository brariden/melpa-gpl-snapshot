# -*- coding: utf-8 -*-
#
# Copyright (c) 2011 Christian Wittern cwittern at gmail.com 
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#
#     * Redistributions in binary form must reproduce the above
#     copyright notice, this list of conditions and the following
#     disclaimer in the documentation and/or other materials provided
#     with the distribution.
#
#     * Neither the name 'Mandoku' nor the names of the contributors
#     may be used to endorse or promote products derived from this
#     software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
"""\
Move punctuation from file 1 to file two.

:author:       Christian Wittern (cwittern[at]gmail.com)
:organization: Mandoku project (http://www.mandoku.org)
:license:      BSD License
"""

from mandoku import *
import sys, codecs
t1='/Users/chris/db/work/DZ1039/DZ1039-000.txt'
#t1='/Users/chris/db/text/cbeta/T/T51n2076/T51n2076-001.txt'
t2='/Users/chris/sb/txt/T51n2076/T51n2076-001.txt'
t2='/Users/chris/db/work/DZ1039.txt'
#t1 = sys.argv[1]
#t2 = sys.argv[2]
f1 = MandokuText(t1)
f1.read()

f2 = MandokuText(t2)
f2.read()

f= MandokuComp(f1)
f.setothertext(f2)
