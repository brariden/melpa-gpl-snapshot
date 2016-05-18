from datetime import datetime, timedelta
from json import dumps
from os import getcwd, unlink
from os.path import join, exists
from time import mktime

import pytest

import tern_django


# Stub tern_django settings.


tern_django.database_file = join(getcwd(), 'tern-django.sqlite')


# Constants.


project = join(getcwd(), '.project')
ext = join(getcwd(), 'test', 'ext')

independent_app = join(project, 'independent')
independent_app_project = join(independent_app, tern_django.tern_file)
independent_app_js = join(
    independent_app, 'static', 'independent', 'independent.js')

static_tag_app = join(project, 'static_tag')
static_tag_app_project = join(static_tag_app, tern_django.tern_file)

use_jquery_app = join(project, 'use_jquery')

cached_app = join(project, 'cached')
cached_app_html = join(
    cached_app, 'templates', 'cached', 'use_underscore.html')

bad_src_app = join(project, 'bad_src')

rendering_app = join(project, 'rendering')

japanese_app = join(project, 'japanese')

use_backbone_app = join(project, 'use_backbone')
use_backbone_app_html = join(
    use_backbone_app, 'templates', 'use_backbone', 'use_backbone.html')

backbone_js = join(ext, 'backbone.min.js')
backbone_url = 'http://backbonejs.org/backbone-min.js'
backbone_sha256 = (
    '75d28344b1b83b5fb153fc5939bdc10b404a754d93f78f7c1c8a8b81de376825')


# Helpers.


def make_timestamp(**kwargs):
    """Create timestamp.

    Keyword arguments passed directly to the timedelta constructor.
    This timedelta will be added to the datetime now.  By default
    it will return current timestamp.
    """

    given_time = datetime.now() + timedelta(**kwargs)
    time_tuple = given_time.timetuple()
    return mktime(time_tuple)


# Fixtures.


@pytest.fixture
def no_tern_projects():
    """Remove all created tern projects before test run."""

    for app in tern_django.applications():
        project = join(app, tern_django.tern_file)
        if exists(project):
            unlink(project)


@pytest.fixture(autouse=True)
def no_urlopen(monkeypatch):
    """Fail all http requests with URLError."""

    def raise_url_error(url, *args, **kwargs):
        raise tern_django.URLError('Monkey patch.')
    monkeypatch.setattr(tern_django, 'urlopen', raise_url_error)


@pytest.fixture(autouse=True)
def db_rollback(request):
    """Rollback any db change after test execution."""

    request.addfinalizer(tern_django.drop_cache)
    tern_django.init_cache()


@pytest.fixture(autouse=True)
def random_storage(tmpdir, monkeypatch):
    """Chose storage directory randomly for each test."""

    monkeypatch.setattr(tern_django, 'storage', tmpdir.strpath)


# Applications.


def test_iter_django_applications():
    """Check we can iterate through django applications."""

    assert all([exists(join(app, '__init__.py'))
                for app in tern_django.applications()])


# Tern project creation.


def test_write_tern_project(no_tern_projects):
    """Check we write tern project in django applications."""

    tern_django.update_tern_projects()
    for app in tern_django.applications():
        has_static = exists(join(app, 'static'))
        has_tern = exists(join(app, tern_django.tern_file))
        assert has_static == has_tern


def test_does_not_modify_existed_files(capsys, no_tern_projects):
    """Check we doesn't overwrite up to date tern projects."""

    with open(independent_app_project, 'w') as project:
        project.write(dumps(tern_django.default_tern_project))
    tern_django.update_tern_projects()
    out, err = capsys.readouterr()
    assert independent_app_project not in out


# Templates analyze.


def test_find_static_files_from_other_application():
    """Check we can search in templates static files include."""

    project = tern_django.analyze_templates(static_tag_app)
    assert project == {'libs': [], 'loadEagerly': [independent_app_js]}


def test_find_static_predefined_libraries():
    """Check we can detect predefined libraries in templates files."""

    project = tern_django.analyze_templates(use_jquery_app)
    assert project == {'libs': ['jquery'], 'loadEagerly': []}


def test_skip_inline_script_tags():
    """Ignore inline html script tags."""

    project = tern_django.analyze_templates(bad_src_app)
    assert project == {'libs': [], 'loadEagerly': []}


def test_meaningful_template():
    """Test if we need process specified template."""

    assert tern_django.meaningful_template('<script>function () {};</script>')
    assert not tern_django.meaningful_template('<body></body>')


def test_needs_to_be_rendered():
    """Test if we need render specified template."""

    assert tern_django.needs_to_be_rendered('<h1>{% load staticfiles %}</h1>')
    assert not tern_django.needs_to_be_rendered('<body></body>')


def test_template_rendering():
    """Test we can render any template."""

    project = tern_django.analyze_templates(rendering_app)
    assert project == {'libs': [], 'loadEagerly': [independent_app_js]}


def test_template_unicode_processing():
    """Check we can open and render templates written in different languages.
    """

    # This must pass without UnicodeError exceptions.
    tern_django.analyze_templates(rendering_app)


# Sql cache.


def test_create_cache_file():
    """Check we create database file."""

    with tern_django.Cache():
        assert exists(tern_django.database_file)


def test_html_cache_table_operations():
    """Check we can create, read and write to html_cache table."""

    html_file = '/test/file.html'
    html_args = (1415483694.061135, '["jquery"]', '')
    tern_django.set_html_cache(html_file, *html_args)
    obtained = tern_django.get_html_cache(html_file)
    assert obtained == html_args


def test_html_cache_table_insert_or_update():
    """Check that set_html_cache will insert record if missed and update record
    if it already present.
    """

    file_name = '/test/me/twice.html'
    params = (1415483694.061135, '["underscore"]', '')
    tern_django.set_html_cache(file_name, *params)
    tern_django.set_html_cache(file_name, *params)
    assert params == tern_django.get_html_cache(file_name)


def test_url_cache_table_operations():
    """Check we can create, read and write to url cache table."""

    url, sha = 'http://example.com', 'nthotnhunoteh'
    tern_django.set_url_cache(url, sha)
    assert sha == tern_django.get_url_cache(url)


# Cache integration with templates analyze.


def test_skip_already_analyzed_template():
    """Check we will ignore templates analyzed earlier."""

    tern_django.set_html_cache(
        cached_app_html, make_timestamp(), '["jquery"]', '')
    project = tern_django.analyze_templates(cached_app)
    assert project == {'libs': ['jquery'], 'loadEagerly': []}


def test_save_analyzed_template_data():

    timestamp = make_timestamp(hours=-1)
    tern_django.set_html_cache(cached_app_html, timestamp, '["jquery"]', '')
    tern_django.analyze_templates(cached_app)
    _, libs, _ = tern_django.get_html_cache(cached_app_html)
    assert '["underscore"]' == libs


def test_skip_caching_template_on_download_error():
    """We must ignore any template caching if we fail to download
    its libraries."""

    tern_django.analyze_templates(use_backbone_app)
    cached = tern_django.get_html_cache(use_backbone_app_html)
    assert not cached


# Libraries download.


def test_download_external_libraries():
    """Check we can download libraries external from internet."""

    tern_django.urlopen = lambda url: open(backbone_js)
    project = tern_django.analyze_templates(use_backbone_app)
    stored_file_path = join(tern_django.storage, backbone_sha256)
    stored_file = open(stored_file_path).read()
    fixture_file = open(backbone_js).read()
    assert stored_file == fixture_file
    assert project == {'libs': [], 'loadEagerly': [stored_file_path]}


def test_skip_already_downloaded_libraries():
    """Check we will use cached sha256 value from cache."""

    url = 'http://example.com'
    sha = 'nthotnhunoteh'
    stored = join(tern_django.storage, sha)
    open(stored, 'a').close()   # Touch a file.
    tern_django.set_url_cache(url, sha)
    assert stored == tern_django.download_library(url)


def test_save_downloaded_library_hash():
    """Check we save downloaded libraries sha256 hashes into url_cache."""

    tern_django.urlopen = lambda url: open(backbone_js)
    tern_django.download_library(backbone_url)
    assert backbone_sha256 == tern_django.get_url_cache(backbone_url)


def test_download_library_output_format():
    """Check that we obtain same results whenever we got if from url_cache or
    download it to storage manually."""

    tern_django.urlopen = lambda url: open(backbone_js)
    downloaded = tern_django.download_library(backbone_url)
    from_cache = tern_django.download_library(backbone_url)
    assert downloaded == from_cache


def test_download_library_ignore_cached_results_missed_from_storage():
    """Check that we will download libraries missed from storage even if we
    got cached results for it."""

    tern_django.set_url_cache(backbone_url, 'is-not-really-a-file')
    tern_django.urlopen = lambda url: open(backbone_js)
    tern_django.download_library(backbone_url)
    assert exists(join(tern_django.storage, backbone_sha256))
    assert backbone_sha256 == tern_django.get_url_cache(backbone_url)
