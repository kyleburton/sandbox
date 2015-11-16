from setuptools import setup, Extension

spammodule = Extension('spam', sources = ['spammodule.c'])

setup (
    name = 'Spam',
    version = '1.0',
    description = 'This is my spam module',
    test_suite = "test.test_spam",
    ext_modules = [spammodule])
