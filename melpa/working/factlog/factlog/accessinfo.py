# Copyright (c) 2013- Takafumi Arakaki

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from .utils.strutils import remove_prefix, get_lines_at_point
from .filetitle import write_path_and_title


class AccessInfo(object):

    """
    Access information object.
    """

    __slots__ = ['path', 'point', 'recorded', 'type', 'showpath']

    def __init__(self, path, point, recorded, type):
        self.path = self.showpath = path
        self.point = point
        self.recorded = recorded
        self.type = type

    def _set_relative_path(self, absunder):
        """
        Set :attr:`showpath` and return the newly set value.

        :attr:`showpath` is set the relative path of :attr:`path` from
        one of the path in `absunder`.

        """
        self.showpath = remove_prefix(absunder, self.path)
        return self.showpath

    def _get_lines_at_point(self, pre_lines, post_lines):
        with open(self.path) as f:
            return get_lines_at_point(
                f.read(), self.point, pre_lines, post_lines)

    def write_paths_and_lines(self, file, pre_lines=0, post_lines=0,
                              newline='\n', separator=':'):
        """
        Write :attr:`showpath` and lines around :attr:`point` to `file`.
        """
        for (lineno, line) in self._get_lines_at_point(pre_lines, post_lines):
            file.write(self.showpath)
            file.write(separator)
            file.write(str(lineno))
            file.write(separator)
            file.write(line)
            file.write(newline)

    def write_path_and_title(self, file, newline='\n', separator=':',
                             **kwds):
        """
        Call :func:`.filetitle.write_path_and_title`.
        """
        write_path_and_title(file, self.path, self.showpath,
                             newline, separator, **kwds)
