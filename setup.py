import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="pycpp", 
    version="1.0.0",
    author="Example Author",
    author_email="na",
    description="CPP/QPP calculator",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/rsi-models/cpp",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: TBD",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)