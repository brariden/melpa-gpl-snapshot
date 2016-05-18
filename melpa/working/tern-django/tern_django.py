#!/usr/bin/env python

"""
    tern_django
    ~~~~~~~~~~~

    This is tern project generator for django apps.

    :copyright: (c) 2014-2016 by Artem Malyshev.
    :license: GPL3, see LICENSE for more details.
"""

import logging
import multiprocessing
import re
import sqlite3
import sys
from hashlib import sha256
try:
    from html.parser import HTMLParser, HTMLParseError
except ImportError:
    from HTMLParser import HTMLParser, HTMLParseError
from json import dumps, loads
from os import makedirs, walk
from os.path import (
    abspath, basename, dirname, exists, expanduser, getmtime, join)
try:
    from urllib.parse import urlsplit
except ImportError:
    from urlparse import urlsplit
try:
    from urllib.request import urlopen, URLError
except ImportError:
    from urllib2 import urlopen, URLError

import django
from django.conf import settings
from django.core.exceptions import ValidationError
from django.core.validators import URLValidator
from django.template import Template, Context


logger = multiprocessing.get_logger()

django_version = django.get_version()
tern_file = '.tern-project'

default_tern_project = {
    'libs': ['browser', 'ecma5'],
    'loadEagerly': ['static/**/*.js'],
}


def run_tern_django():
    """Perform django project analysis.
    Basically program entry point.
    """

    init_logging()
    init_cache()
    update_tern_projects()


def init_logging():
    """Initialize logging system."""

    debug = '--debug' in sys.argv
    level = logging.DEBUG if debug else logging.INFO
    multiprocessing.log_to_stderr()
    logger.setLevel(level)


def initialize():
    """Initialize django applications."""

    if django_version >= '1.7':
        django.setup()


def applications():
    """Collect directories with django applications."""

    initialize()
    if django_version >= '1.7':
        from django.apps import apps
        return [app.path for app in apps.get_app_configs()]
    else:
        from django.utils.importlib import import_module
        from django.utils._os import upath
        directories = []
        apps = settings.INSTALLED_APPS
        for app in apps:
            mod = import_module(app)
            mod_path = dirname(upath(mod.__file__))
            directories.append(mod_path)
        return directories


# Tern project saving.


def update_tern_projects():
    """Update tern projects in each django application."""

    pool = multiprocessing.Pool(processes=multiprocessing.cpu_count() * 2)
    try:
        pool.map(update_application, applications())
    finally:
        pool.close()
        pool.join()


def update_application(app):
    """Update tern project in specified django application."""

    try:
        initialize()                # One more time for child process.
        static = join(app, 'static')
        if exists(static):
            project_file = join(app, tern_file)
            templates_tern_project = analyze_templates(app)
            tern_project = merge_projects(
                default_tern_project,
                templates_tern_project)
            save_tern_project(tern_project, project_file)
    except Exception as error:
        logger.exception('Unexpected error occurs: %s', error)
        raise


def merge_projects(*projects):
    """Merge non empty projects all together."""

    non_empty = list(filter(None, projects))  # We need list to reuse it.
    keys = set((k for project in non_empty for k in project))
    return dict(((k, list(set((v
                               for project in non_empty
                               for v in project[k]))))
                 for k in keys))


def save_tern_project(tern_project, project_file):
    """Save tern project to specified file if necessary."""

    if not exists(project_file):
        write_tern_project(tern_project, project_file)
        return
    with open(project_file) as project:
        written_project = loads(project.read())
    if written_project != tern_project:
        write_tern_project(tern_project, project_file)


def write_tern_project(tern_project, project_file):
    """Save tern project."""

    logger.info('Write tern project to %s', project_file)
    with open(project_file, 'w') as project:
        project.write(dumps(tern_project))


# Templates analyze.


def analyze_templates(app):
    """Add to project properties grabbed from app templates."""

    projects = []
    templates = join(app, 'templates')
    for root, dirs, files in walk(templates):
        htmls = [join(root, f) for f in files if f.endswith('.html')]
        for html in htmls:
            projects.append(process_html_template(html, app))
    return merge_projects(*projects)


def process_html_template(html, app):
    """Grab static files from html template."""

    logger.debug('Process template: {0}'.format(html))

    cached = get_template_cache(html)
    if cached:
        libs, loadEagerly = cached
        return {'libs': libs, 'loadEagerly': loadEagerly}

    with open(html, 'rb') as template:
        source = template.read().decode(settings.FILE_CHARSET)

    if not meaningful_template(source):
        set_template_cache(html)
        return

    try:
        libs, loadEagerly = parse_template(source, app)
    except URLError:
        pass                    # Fail to download external library.
    else:
        set_template_cache(html, libs, loadEagerly)
        return {'libs': libs, 'loadEagerly': loadEagerly}


def get_template_cache(html_file):
    """Check database cache for html file information."""

    cache = get_html_cache(html_file)
    if cache:
        cache_mtime, cache_libs, cache_eagerly = cache
        mtime = getmtime(html_file)
        if mtime < cache_mtime:
            libs = loads(cache_libs) if cache_libs else []
            loadEagerly = loads(cache_eagerly) if cache_eagerly else []
            return libs, loadEagerly


def set_template_cache(html_file, libs=None, loadEagerly=None):
    """Save html file information into database cache."""

    mtime = getmtime(html_file)
    set_html_cache(html_file, mtime, dumps(libs), dumps(loadEagerly))


def parse_template(source, app):
    """Parse html source string.  Save information in the project."""

    rendered_source = render_template_if_necessary(source)
    try:
        parser = TemplateParser()
        # Don't move this to TemplateParser init.  Super will not
        # properly work with this class in python2.x
        parser.src = []
        parser.feed(rendered_source)
    except HTMLParseError:
        pass
    analyzer = TemplateAnalyzer(app, parser.src)
    analyzer.find()
    return analyzer.libs, analyzer.loadEagerly


class TemplateParser(HTMLParser):
    """Static files html grabber."""

    def handle_starttag(self, tag, attrs):
        """Process script html tags."""

        if tag == 'script':
            for attr, value in attrs:
                if attr == 'src':
                    self.src.append(value)


class TemplateAnalyzer(object):
    """Analyze static files source."""

    def __init__(self, app, sources):

        self.sources = sources
        self.app = app
        self.libs = []
        self.loadEagerly = []

    def find(self):
        """Find true definitions of each source."""

        for src in self.sources:
            self.handle(src)

    def handle(self, src):
        """Determine and save true src location."""

        if self.is_relative(src):
            self.process_relative_url(src)
        else:
            self.process_absolute_url(src)

    def is_relative(self, src):
        """Check if src specify resource from same domain."""

        return src.startswith('/')

    def process_relative_url(self, uri):
        """Find static file from its uri."""

        file_base = uri.replace(settings.STATIC_URL, '')
        for app in applications():
            path = abspath(join(app, 'static', file_base))
            if exists(path) and not path.startswith(self.app):
                self.loadEagerly.append(path)
                break

    def process_absolute_url(self, url):
        """Find external library.  Download if needed."""

        if self.validate_absolute_url(url):
            scheme, netloc, path, query, fragment = urlsplit(url)
            base = basename(path)
            for lib in ['jquery', 'underscore']:
                if base.startswith(lib):
                    self.libs.append(lib)
                    break
            else:
                stored_lib = download_library(url)
                self.loadEagerly.append(stored_lib)

    def validate_absolute_url(self, url):
        """Check that url formed correctly."""

        validator = URLValidator()
        try:
            validator(url)
        except ValidationError:
            pass
        else:
            return True


def meaningful_template(template):
    """Check if template is interesting for tern."""

    # Don't close first script tag here.
    return '<script' in template and '</script>' in template


# Template rendering.


template_tag_regex = re.compile(r'{%.*%}|{{.*}}|{#.*#}')
static_tag_regex = re.compile(r'{%\s*static\s*.*\s*%}')


def render_template_if_necessary(source):
    """Render django template if necessary."""

    if needs_to_be_rendered(source):
        rendered_source = template_tag_regex.sub(render_node, source)
        return rendered_source
    else:
        return source


def render_node(node):
    """Render one template node.
    Templates node has represented by regex match objects.
    We have interest in static template tags only so we will omit other tags.
    """

    token = node.group(0)
    if static_tag_regex.match(token):
        template = Template('{% load staticfiles %}' + token)
        context = Context({})
        try:
            rendered = template.render(context)
        except Exception:
            return ''               # Ignore any rendering error.
        else:
            return rendered
    else:
        return ''               # Ignore any other tags.


def needs_to_be_rendered(template):
    """Check if template has necessary django tags"""

    return '{% load staticfiles %}' in template


# Sql cache.


database_file = expanduser('~/.emacs.d/tern-django.sqlite')


class Cache(object):
    """Tern django database cache."""

    def __init__(self):

        self.connection = None

    def __enter__(self):
        """Connect to cache database."""

        self.connection = sqlite3.connect(database_file)
        return self.connection.__enter__()

    def __exit__(self, type, value, traceback):
        """Disconnect from cache database."""

        if self.connection is not None:
            self.connection.__exit__(type, value, traceback)
            self.connection.close()
        self.connection = None


def init_cache():
    """Create cache tables if necessary."""

    with Cache() as connection:
        connection.executescript("""
        create table if not exists html_cache (
            "id" integer primary key,
            "file_name" text unique not null,
            "mtime" real,
            "libs" text,
            "loadEagerly" text);
        create table if not exists url_cache (
            "id" integer primary key,
            "url" text unique not null,
            "sha256" text not null);
        """)


def drop_cache():
    """Drop cache tables if necessary."""

    with Cache() as connection:
        connection.executescript("""
        drop table if exists html_cache;
        drop table if exists url_cache;
        """)


def get_html_cache(file_name):
    """Get file name attributes from cache if exists."""

    with Cache() as connection:
        cursor = connection.execute("""
        select "mtime", "libs", "loadEagerly"
        from html_cache
        where file_name=?;
        """, (file_name,))
        return cursor.fetchone()


def set_html_cache(file_name, mtime, libs, loadEagerly):
    """Set file name attributes in cache."""

    if get_html_cache(file_name):
        query = """
        update html_cache
        set "mtime"=:mtime, "libs"=:libs, "loadEagerly"=:loadEagerly
        where "file_name"=:file_name;
        """
    else:
        query = """
        insert into html_cache("file_name", "mtime", "libs", "loadEagerly")
        values (:file_name, :mtime, :libs, :loadEagerly);
        """
    params = {
        'file_name': file_name,
        'mtime': mtime,
        'libs': libs,
        'loadEagerly': loadEagerly,
    }
    with Cache() as connection:
        connection.execute(query, params)


def get_url_cache(url):
    """Get sha256 for file at given placed url if exists."""

    with Cache() as connection:
        cursor = connection.execute("""
        select "sha256"
        from url_cache
        where "url"=?;
        """, (url,))
        received = cursor.fetchone()
        if received:
            return received[0]


def set_url_cache(url, sha256):
    """Set sha256 value for file placed at given url."""

    with Cache() as connection:
        if get_url_cache(url):
            query = """
            update url_cache set "sha256"=:sha256
            where "url"=:url;
            """
        else:
            query = """
            insert into url_cache("url", "sha256")
            values (:url, :sha256);
            """
        connection.execute(query, {'url': url, 'sha256': sha256})


# Libraries download.


storage = expanduser('~/.emacs.d/tern-django-storage')


def create_storage():
    """Create storage directory if necessary."""

    try:
        makedirs(storage)
    except OSError:
        pass                    # Storage exists.


def download_library(url):
    """Download library if necessary."""

    cached = get_url_cache(url)
    if cached:
        stored_library = join(storage, cached)
        if exists(stored_library):
            return stored_library

    create_storage()
    try:
        response = urlopen(url)
    except URLError:
        logger.error('Fail to download external library: %s', url)
        raise
    else:
        logger.info('Download external library: %s', url)
    content = response.read()
    hexdigest = content_hash(content)
    file_path = join(storage, hexdigest)
    if not exists(file_path):
        with open(file_path, 'w') as cache_file:
            cache_file.write(content)
    set_url_cache(url, hexdigest)
    return file_path


def content_hash(content):
    """Count sha256 hash for content."""

    content_hash = sha256()
    content_hash.update(content.encode())
    hexdigest = content_hash.hexdigest()
    return hexdigest


if __name__ == '__main__':
    run_tern_django()
