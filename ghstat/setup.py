from setuptools import setup
setup(
    name = "ghstat",
    version = "0.1",
    description = "Commandline dashboard to see latest state of pull requests for a repository",
    author = "Yoriyasu Yano",
    author_email = "yorinasub17@gmail.com",


    scripts = ['ghstat'],
    install_requires = open('requirements.txt').read().split()
)
