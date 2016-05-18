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


import sys
import itertools

from .config import ConfigStore
from .database import DataBase
from .utils.iterutils import interleave


def get_db(*args, **kwds):
    config = ConfigStore(*args, **kwds)
    return DataBase(config.db_path)


def record_add_arguments(parser):
    parser.add_argument(
        'file_path',
        help="record an activity on this file.")
    parser.add_argument(
        '--access-type', '-a', default='write',
        choices=DataBase.ACCESS_TYPES,
        help="how the file is accessed.")
    parser.add_argument(
        '--file-point', type=int,
        help="point of cursor at the time of saving.")
    parser.add_argument(
        '--program',
        help="program used for accessing file.")


def record_run(file_path, file_point, access_type, program):
    """
    Record activities on file.
    """
    db = get_db()
    db.record_file_log(
        file_path, file_point=file_point, access_type=access_type,
        program=program)


def list_add_arguments(parser):
    import argparse
    parser.add_argument(
        '--limit', '-l', type=int, default=20,
        help="Maximum number of files to list.")
    parser.add_argument(
        '--access-type', '-a', dest='access_types',
        action='append', choices=DataBase.ACCESS_TYPES,
        help="""
        Access types to include.
        This option can be called multiple times.
        Default is to include all activities.
        """)
    parser.add_argument(
        '--no-unique', dest='unique', action='store_false', default=True,
        help="""
        Include all duplicates.  See also --format.
        """)
    parser.add_argument(
        '--include-glob', '-g', metavar='GLOB', default=[], action='append',
        help="""
        Include only paths that match to unix-style GLOB pattern.
        """)
    parser.add_argument(
        '--exclude-glob', '-G', metavar='GLOB', default=[], action='append',
        help="""
        Exclude paths that match to unix-style GLOB pattern.
        """)
    parser.add_argument(
        '--program', default=[], action='append',
        help="""
        List of program to be used to access the file.
        """)
    parser.add_argument(
        '--file-exists',
        dest='file_exists', default=None, action='store_true',
        help="""
        Include only files existed at *recording* time.
        """)
    parser.add_argument(
        '--no-file-exists',
        dest='file_exists', default=None, action='store_false',
        help="""
        Include only files not existed at *recording* time.
        """)
    parser.add_argument(
        '--under', '-u', metavar='PATH', default=[], action='append',
        help="""
        Show only paths under PATH.  See also: --relative.
        """)
    parser.add_argument(
        '--relative', action='store_true',
        help="""
        Output paths relative to the one given by --under.
        """)
    parser.add_argument(
        '--format',
        help="""
        [WORK IN PROGRESS]
        Python-style string format.  {path}: file path; {point}:
        cursor point; {recorded}: timestamp; {access}: one of
        'open', 'write' and 'close'; {id}: row id.
        """)
    parser.add_argument(
        '--title', action='store_true',
        help="""
        Output title of the file.  Supported file formats are:
        Python, reStructuredText, Markdown, Org-mode.
        It does not work with --line-number.
        """)
    parser.add_argument(
        '--line-number', action='store_true',
        help="""
        [WORK IN PROGRESS]
        Output the line of the file where the cursor located
        at the time of the action.
        Prefix each output line with the 1-based line number,
        like grep's --line-number.
        """)
    parser.add_argument(
        '--after-context', '-A', type=int, metavar='NUM',
        help="""
        Print NUM lines after the cursor line.
        It requires --line-number.
        """)
    parser.add_argument(
        '--before-context', '-B', type=int, metavar='NUM',
        help="""
        Print NUM lines before the cursor line.
        It requires --line-number.
        """)
    parser.add_argument(
        '--context', '-C', type=int, metavar='NUM',
        help="""
        Print NUM lines before and after the cursor line.
        It requires --line-number.
        """)
    parser.add_argument(
        '--null', action='store_true',
        help="""
        Use the NULL character instead of newline for separating
        files.
        """)
    parser.add_argument(
        '--output', default='-', type=argparse.FileType('w'),
        help='file to write output. "-" means stdout.')


def list_run(
        limit, access_types, unique, include_glob, exclude_glob,
        file_exists, program,
        under, relative, null, **kwds):
    """
    List recently accessed files.
    """
    newline = '\0' if null else '\n'
    db = get_db()
    rows = db.search_file_log(
        limit=limit, access_types=access_types, unique=unique,
        include_glob=include_glob, exclude_glob=exclude_glob,
        file_exists=file_exists, program=program,
        under=under, relative=relative)
    write_listed_rows(rows, newline, **kwds)


def write_listed_rows(
        rows, newline, output, title,
        before_context, after_context, context, **_):
    r"""
    Write `rows` into `output`.

    :type            rows: iterative of :class:`factlog.database.AccessInfo`
    :arg             rows:
    :type         newline: str
    :arg          newline: '\n' or '\0'
    :type           title: bool
    :arg            title:
    :type  before_context: int or None
    :arg   before_context: print this number of line before the point
    :type   after_context: int or None
    :arg    after_context: print this number of line after the point
    :type         context: int or None
    :arg          context: print this number of line before and after the point

    """
    nonnone = lambda x: x is not None
    if title:
        for info in rows:
            info.write_path_and_title(output, newline)
    elif list(filter(nonnone, [before_context, after_context, context])):
        pre_lines = next(iter(filter(nonnone, [before_context, context, 0])))
        post_lines = next(iter(filter(nonnone, [after_context, context, 0])))
        for info in rows:
            info.write_paths_and_lines(output, pre_lines, post_lines,
                                       newline)
    else:
        showpaths = (r.showpath for r in rows)
        output.writelines(interleave(showpaths, itertools.repeat(newline)))
    if output is not sys.stdout:
        output.close()


commands = [
    ('record', record_add_arguments, record_run),
    ('list', list_add_arguments, list_run),
]
