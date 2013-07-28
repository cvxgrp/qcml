from distutils.core import setup

setup(
    name='qcml',
    version='0.0.1',
    author='Eric Chu',
    author_email='eytchu@gmail.com',
    packages=[  'qcml',
                'qcml.qc_ply',
                'qcml.atoms',
                'qcml.expressions',
                'qcml.properties',
                'qcml.codegens',
                'qcml.test'],
    package_dir={'qcml': 'python'},
        url='http://cvxgrp.github.io/qcml/',
    license='BSD',
    description='A parser for modeling convex optimization problems in Python.',
    long_description=open('../README.md').read(),
    requires = ["cvxopt(>= 1.1.5)",
                "pdos(>= 1.0)",
                "ecos(>=1.0)"] # this doesn't appear to do anything unfortunately
)
