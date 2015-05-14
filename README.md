This tool compares a cabal packages local working directory against
its counterpart on hackage and reports any differences.

The basic goal is to provide a simple tool which allows you to
identify which packages are different locally from hackage and may
need to be uploaded.

This tool works as follows:

 1. read the local `.cabal` file and figure out the package name
 2. use 'cabal fetch' to get the latest version of the package from hackage
 3. use 'cabal sdist' to generate the `.tar.gz` for the local working directory
 4. untar both `.tar.gz` bundles into temporary directories
 5. use `diff -ruN` to check for differences
 6. exit with 0 if no differences found
 7. exit with 1 if differences with found
 8. exit with 2 if other errors encountered


