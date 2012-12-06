from distutils.core import setup

setup(
    name='alppng',
    version='0.1.0',
    author='Guillaume Lathoud',
    author_email='Guillaume.Lathoud@alpstein.de',
    packages=['alppng'],
    scripts=['inplace_green2transp', 'check_transparent'],
    url='https://github.com/Alpstein/alppng',
    license='LICENSE.md',
    description='Convert green tiles to transparent tiles',
    long_description=open('README.md').read(),
)
