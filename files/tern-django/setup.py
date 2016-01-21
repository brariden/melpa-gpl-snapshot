from setuptools import setup

readme = open('README.rst').read()

setup(name='tern_django',
      version='0.1.0',
      url='https://github.com/proofit404/tern-django',
      description='Create Tern projects for Django applications.',
      long_description=readme,
      platforms='any',
      license='GPL3',
      author='Artem Malyshev',
      author_email='proofit404@gmail.com',
      maintainer='Artem Malyshev',
      maintainer_email='proofit404@gmail.com',
      py_modules=['tern_django'],
      entry_points={
          'console_scripts': [
              'tern_django=tern_django:run_tern_django',
          ],
      },
      classifiers=[
          'Development Status :: 4 - Beta',
          'Environment :: Console',
          'Framework :: Django',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
          'Programming Language :: JavaScript',
          'Programming Language :: Python :: 2',
          'Programming Language :: Python :: 2.6',
          'Programming Language :: Python :: 2.7',
          'Programming Language :: Python :: 3',
          'Programming Language :: Python :: 3.2',
          'Programming Language :: Python :: 3.3',
          'Programming Language :: Python :: 3.4',
          'Topic :: Text Editors',
          'Topic :: Text Editors :: Emacs',
      ])
