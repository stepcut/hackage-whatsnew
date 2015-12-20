hackage-whatsnew [![Hackage](https://img.shields.io/hackage/v/hackage-whatsnew.svg)](https://hackage.haskell.org/package/hackage-whatsnew) [![Build Status](https://api.travis-ci.org/stepcut/hackage-whatsnew.svg?branch=master)](https://travis-ci.org/stepcut/hackage-whatsnew)
================

Do you often modify a package, check that it builds in travis, and then FORGET TO UPLOAD IT!

Then this tool is for you! Its raison d'etre is to see if you have
changes in your local directory which are not on hackage.

Usage
-----

`hackage-whatsnew` depends on the following executables:

 1. cabal

 2. tar

 3. GNU diff (or any `diff` which supports the `-r`, `-u`, and `-N` options)

These binaries need to be in the current search path. Assuming
everything is installed, to use `hackage-whatsnew` you simply need to:

 1. run `cabal update`

 2. cd into the same directory as the `.cabal` file

 3. run `hackage-whatsnew`

If no changes are detected, then nothing is printed and the exit code is 0.

If changes are detected a recursive diff is displayed and the exit code is 1.

How It Works
------------

This tool works as follows:

 1. read the local `.cabal` file and figure out the package name

 2. use `cabal fetch` to get the latest version of the package from hackage

 3. use `cabal sdist` to generate the `.tar.gz` for the local working directory

 4. untar both `.tar.gz` bundles into temporary directories

 5. use `diff -ruN` to check for differences

 6. exit with 0 if no differences found

 7. exit with 1 if differences with found

 8. exit with 2 if other errors encountered


FAQ
---

**Q**: Why is it called `hackage-whatsnew` instead of `hackage-diff`?

**A**: Because `hackage-diff` was already taken. The `whatsnew` term is inspired by `darcs whatsnew`.

**Q**: Would it by great if the tool did XYZ?

**A**: Yes! Please submit a pull request.
