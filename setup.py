import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="srp",
    version="0.2.0",
    author="Francois Laliberte-Auger, Pierre-Carl Michaud",
    author_email="francois.laliberte-auger@hec.ca",
    description="Module to simulate CPP and QPP",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/rsi-models/srp",
    packages=setuptools.find_packages(),
    include_package_data=True,
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
