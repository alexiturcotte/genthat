[![Build
Status](https://travis-ci.org/PRL-PRG/genthat.svg)](https://travis-ci.org/PRL-PRG/genthat)
[![codecov](https://codecov.io/github/PRL-PRG/genthat/branch/feature/fixes/graphs/badge.svg)](https://codecov.io/github/PRL-PRG/genthat)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/genthat)](https://cran.r-project.org/package=genthat)

While unit tests are great for getting some basic guarantees about code,
writing them is never fun. It is both time consuming and error prone
because of lots of code repetition involved in sending the same, or
similar arguments to the functions with only minimal variation over the
different tests.

Genthat offers a different approach. Given the functions for which one
wants to create unit tests and a code that actually uses them (e.g., a
vignette, an examples, or just some arbitrary R code), genthat executes
the code while intercepting all the functions calls and recording their
input arguments and return values. From these recordings it then
generates [testthat](https://github.com/r-lib/testthat) unit tests for
all of the captured calls. These unit tests can be further pruned so to
keep only those that increase the original functions’ code coverage.

Genthat does not magically create new tests out of the blue, instead it
simply extracts the smallest possible test fragments from existing
codes. The produced tests are therefore only as good as the code used to
generate them. They cannot determine whether the code behaves correctly,
only whether it behaves consistently since the time the tests were
generated. However, this already significantly helps with bug fixing.
The generated tests concentrate on single functions and test them
independently of each other. Therefore a failing test usually locates
the error more precisely that a failing chunk of application code.

Installation
------------

The package shall soon be on CRAN, in the mean time, it can be installed
easily using the `devtools` package:

    devtools::install_github('genthat', 'PRL-PRG')

Or download the sources and build manually. If you’re running R on
Windows, you need to install Rtools.

Usage
-----

-   [Overview]() - An overview how to use genthat
-   [Tests from Traces: Automated Unit Test Generation for R]() - a
    paper published at ISSTA’18 about a design of genthat and an
    experiment of running genthat on a large portion of CRAN packages (A
    runnable artifact as docker image is also
    [available](https://github.com/fikovnik/ISSTA18-artifact/))
