from distutils.core import setup

requires=('cvxopt >= 1.1.5')
    
setup(
    name='scoop',
    version='0.0.1',
    author='Eric Chu',
    author_email='eytchu@gmail.com',
    packages=['scoop', 'scoop.codegen', 'scoop.atoms'], # will probably need to call it something else
    package_dir={'scoop': 'python'},
    #scripts=['bin/stowe-towels.py','bin/wash-towels.py'],
    url='http://github.com/cvxgrp/scoop',
    license='BSD',
    description='A parser for modeling convex optimization problems in Python.',
    long_description=open('../README.md').read()
)