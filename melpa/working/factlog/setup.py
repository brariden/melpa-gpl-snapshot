from setuptools import setup

import factlog

setup(
    name='factlog',
    version=factlog.__version__,
    packages=['factlog', 'factlog.utils', 'factlog.tests'],
    author=factlog.__author__,
    author_email='aka.tkf@gmail.com',
    url='https://github.com/tkf/factlog',
    license=factlog.__license__,
    description='factlog - File ACTivity LOGger',
    long_description=factlog.__doc__,
    keywords='history, recently used files',
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Programming Language :: Python",
        "Programming Language :: Python :: 2",
        "Programming Language :: Python :: 2.6",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.2",
        # see: http://pypi.python.org/pypi?%3Aaction=list_classifiers
    ],
    install_requires=[
        'argparse',
    ],
    entry_points={
        'console_scripts': ['factlog = factlog.cli:main'],
    },
)
