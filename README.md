hackage-whatsnew [![Hackage](https://img.shields.io/hackage/v/hackage-whatsnew.svg)](https://hackage.haskell.org/package/hackage-whatsnew) [![Build Status](https://api.travis-ci.org/Happstack/hackage-whatsnew.svg?branch=master)](https://travis-ci.org/Happstack/hackage-whatsnew)
================

Do you often modify a package, check that it builds in travis, and then FORGET TO UPLOAD IT!

Then this tool is for you! It's raison d'etre is to see if you have
changes in your local directory which are not on hackage.

You should run `cabal update` before running this command.

This tool works as follows:

 1. read the local `.cabal` file and figure out the package name

 2. use `cabal fetch` to get the latest version of the package from hackage

 3. use `cabal sdist` to generate the `.tar.gz` for the local working directory

 4. untar both `.tar.gz` bundles into temporary directories

 5. use `diff -ruN` to check for differences

 6. exit with 0 if no differences found

 7. exit with 1 if differences with found

 8. exit with 2 if other errors encountered



