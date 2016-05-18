#
# -*- coding: utf-8 -*-
#
# Copyright (c) 2016 Lorenzo Villani
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the 'Software'),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

import os.path
import subprocess

import sublime
import sublime_plugin

KEY_METAFMT_ENABLED = 'metafmt_enabled'


class MetafmtBufferCommand(sublime_plugin.TextCommand):

    def run(self, edit):
        whole_buffer = sublime.Region(0, self.view.size())

        buffer_contents = self.view.substr(whole_buffer)
        if not buffer_contents:
            return

        syntax = self.view.settings().get('syntax')
        if not syntax:
            return

        syntax = os.path.splitext(os.path.basename(syntax))[0]

        print('metafmt:', syntax)

        new_contents = self._fmt(syntax, buffer_contents)
        if not new_contents:
            return

        self.view.replace(edit, whole_buffer, new_contents)

    def _fmt(self, syntax, buffer_contents):
        cmd = ['metafmt', '-editor', 'sublime', '-syntax', syntax, '-']
        pipe = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE)

        buf = bytes(buffer_contents, encoding='utf-8')

        try:
            return pipe.communicate(input=buf)[0].decode('utf-8')
        except:
            return ''


class MetafmtToggleFormatOnSaveCommand(sublime_plugin.TextCommand):

    def run(self, *args):
        new_state = not self.view.settings().get(KEY_METAFMT_ENABLED, False)

        self.view.settings().set(KEY_METAFMT_ENABLED, new_state)
        sublime.status_message('Format on save %s' % 'enabled' if new_state else 'disabled')


class MetafmtToggleProjectFormatOnSaveCommand(sublime_plugin.TextCommand):

    def run(self, *args):
        window = self.view.window()

        project_data = window.project_data()
        new_state = not project_data.get('settings', dict()).get(KEY_METAFMT_ENABLED, False)

        project_data['settings'][KEY_METAFMT_ENABLED] = new_state
        window.set_project_data(project_data)

        sublime.status_message('Format on save (project) %s' % 'enabled' if new_state else 'disabled')


class MetafmtBeforeSave(sublime_plugin.EventListener):

    def on_pre_save(self, view):
        if not view.settings().get(KEY_METAFMT_ENABLED, False):
            return

        if not view.is_dirty():
            return

        view.run_command('metafmt_buffer')
