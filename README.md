# VC2copula

<!-- badges: start -->
[![R build status](https://github.com/tnagler/VC2copula/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/tnagler/VC2copula/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/VC2copula)](https://CRAN.R-project.org/package=VC2copula)
[![Codecov status](https://codecov.io/gh/tnagler/VC2copula/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tnagler/VC2copula)
<!-- badges: end -->

------------------------------

VC2copula extends the [copula](https://CRAN.R-project.org/package=copula) 
package with families and models from [VineCopula](https://tnagler.github.io/VineCopula/). 
This functionality was previously part of [VineCopula](https://tnagler.github.io/VineCopula/), 
but is now provided as a stand-alone version.

## Installation

You can install the released version of VC2copula from 
[CRAN](https://CRAN.R-project.org) with

``` r
install.packages("VC2copula")
```
and the development version with

``` r
devtools::install_github("tnagler/VC2copula")
```

------------------------------

## Functionality

### Bivariate copula models

The package exposes classes for all BB- and Tawn families implemented in [VineCopula](http://tnagler.github.io/VineCopula/), including their rotations.
Additionally, rotated versions of the Clayton and Gumbel copula families are exposed. 

#### Example

``` r
cop <- BB1Copula(c(1, 1.5))
dCopula(c(0.5, 0.5), cop)
plot(rCopula(500, cop))
```

#### Full list of exposed classes

*  `"BB1Copula"`, `"surBB1Copula"`, `"r90BB1Copula"`, `"r270BB1Copula"`
*  `"BB6Copula"`, `"surBB6Copula"`, `"r90BB6Copula"`, `"r270BB6Copula"`
*  `"BB7Copula"`, `"surBB7Copula"`, `"r90BB7Copula"`, `"r270BB7Copula"`
*  `"BB8Copula"`, `"surBB8Copula"`, `"r90BB8Copula"`, `"r270BB8Copula"`
*  `"surClaytonCopula"`, `"r90ClaytonCopula"`, `"r270ClaytonCopula"`
*  `"surGumbelCopula"`, `"r90GumbelCopula"`, `"r270GumbelCopula"`
*  `"joeBiCopula"`, `"surJoeBiCopula"`, `"r90JoeBiCopula"`, `"r270JoeBiCopula"`
*  `"tawnT1Copula"`, `"surTawnT1Copula"`, `"r90TawnT1Copula"`, `"r270TawnT1Copula"`
*  `"tawnT2Copula"`, `"surTawnT2Copula"`, `"r90TawnT2Copula"`, `"r270TawnT2Copula"`

### Vine copula models

There is one more class `vineCopula` for vine copula models. You can convert
`RVineMatrix` objects for the VineCopula package or fit new models using only
the copula API:

``` r
data("daxreturns", package = "VineCopula")
pairs(daxreturns[, 1:4])

vine <- vineCopula(as.integer(4))
fit <- fitCopula(vine, daxreturns[, 1:4])
pairs(rCopula(500, fit@copula))
```

------------------------------

## Documentation

For a full overview of functions and classes, see the [package website](https://tnagler.github.io/VC2copula/).


