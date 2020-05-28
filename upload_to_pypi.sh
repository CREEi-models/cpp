python setup.py sdist bdist_wheel 
twine register dist/srp-0.2.0.tar.gz
twine register dist/srp-0.2-py3-none-any.whl
twine upload dist/*