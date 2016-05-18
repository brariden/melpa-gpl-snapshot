from __future__ import print_function
from django.conf import settings
from json import dumps

print(dumps(settings.DATABASES))
