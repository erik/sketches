from setuptools import setup


setup(
    name='timewarp',
    version='0.1.0',
    description='Lisping through time',
    author='Erik Price',
    url='https://github.com/erik/timewarp',
    packages=['timewarp'],
    entry_points={
        'console_scripts': [
            'timewarp = timewarp:main',
        ],
    },
    license='MIT',
    install_requires=['docopt==0.6.2']
)
