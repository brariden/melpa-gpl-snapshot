# -.- coding: utf-8 -.-

# Zeitgeist
#
# Copyright © 2009 Markus Korn <thekorn@gmx.de>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Post-commit hook to submit the commit to Zeitgeist (http://www.zeitgeist-project.com)

Requires bzr 0.15 or higher.

Copyright (C) 2009, Markus Korn <thekorn@gmx.de>
Copyright (C) 2010, Stefano Candori <stefano.candori@gmail.com>
Copyright (C) 2011, Jelmer Vernooij <jelmer@samba.org>
Published under the GNU GPLv2 or later

Installation:
Copy this directory to ~/.bazaar/plugins/zeitgeist/*
"""

from __future__ import absolute_import

from bzrlib import branch

branch.Branch.hooks.install_named_hook_lazy("post_commit",
    "bzrlib.plugins.zeitgeist.hooks", "post_commit",
    "Zeitgeist dataprovider for bzr")
branch.Branch.hooks.install_named_hook_lazy("post_pull",
    "bzrlib.plugins.zeitgeist.hooks", "post_pull",
    "Zeitgeist dataprovider for bzr")
branch.Branch.hooks.install_named_hook_lazy("post_push",
    "bzrlib.plugins.zeitgeist.hooks", "post_push",
    "Zeitgeist dataprovider for bzr")
