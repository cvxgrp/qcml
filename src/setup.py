from distutils.core import setup
    
setup(
    name='scoop',
    version='0.0.1',
    author='Eric Chu',
    author_email='eytchu@gmail.com',
    packages=[  'scoop', 
                'scoop.qc_ply',
                'scoop.codegen', 
                'scoop.atoms', 
                'scoop.expression',
                'scoop.test'],
    package_dir={'scoop': 'python'},
        url='http://github.com/cvxgrp/scoop',
    license='BSD',
    description='A parser for modeling convex optimization problems in Python.',
    long_description=open('../README.md').read(),
    requires = ["cvxopt(>= 1.1.5)", 
                "pdos(>= 1.0)", 
                "numpy(>=1.7)", 
                "ecos(>=1.0)"] # this doesn't appear to do anything unfortunately
)