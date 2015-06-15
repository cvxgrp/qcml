from distutils.core import setup

setup(
    name='qcml',
    version='0.2.0',
    author='Eric Chu',
    author_email='eytchu@gmail.com',
    packages=[  'qcml',
                'qcml.ast',
                'qcml.ast.atoms',
                'qcml.ast.constraints',
                'qcml.ast.expressions',
                'qcml.ast.socps',
                'qcml.properties',
                'qcml.codegens',
                'qcml.codegens.python',
                'qcml.codegens.C',
                'qcml.codegens.matlab',
                'qcml.codegens.cvx',
                'qcml.mixins',
                'qcml.codes',
                'qcml.codes.encoders',
                'qcml.codes.coefficients',
                'qcml.test'],
    package_dir={'qcml': 'src'},
    package_data = {'qcml':
        ['codegens/C/*_template*',
         'codegens/C/qcml_utils.*',
         'test/problems/*.prob']
    },
    url='http://github.com/cvxgrp/qcml/',
    license='BSD',
    description='A parser for modeling convex optimization problems in Python.',
    long_description=open('README.md').read(),
    requires = ["ply(>= 3.4)",
                "ecos(>= 1.0.1)",
                "numpy(>= 1.7)",
                "scipy(>= 0.12)"]
)
