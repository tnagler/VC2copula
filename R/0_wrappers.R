#' Construction of a Copula Object from a VineCopula Family Index
#'
#' A VineCopula family index along with its parameters is used to construct a
#' corresponding [copula-class] object.
#'
#' If the family and parameter specification is stored in a `[BiCop()]` object
#' `obj`, the alternative version
#' ```
#' BiCop2copula(u1, u2, obj)
#' ```
#' can be used.
#' @param family a family index as defined in [VineCopula-package()].
#' @param par first parameter.
#' @param par2 second parameter.
#' @param obj [BiCop()] object containing the family and parameter specification.

#' @return An object inheriting [copula-class] corresponding to the
#' specific family.
#' @aliases copulaFromFamilyIndex
#' @examples
#' # normalCopula with parameter 0.5
#' BiCop2copula(1, 0.5)
#'
#' # rotated Tawn T2 copula
#' BiCop2copula(224, -2, 0.5)
#'
#' @export
#'
BiCop2copula <- function(family, par, par2 = 0, obj = NULL) {
  if (inherits(obj, "BiCop")) {
    cop <- BiCop2copula(obj$family, obj$par, obj$par2)
  } else if (inherits(family, "BiCop")) {
    cop <- BiCop2copula(family$family, family$par, family$par2)
  } else {
    cop <- copulaFromFamilyIndex(family, par, par2)
  }
  cop
}

#' @rdname BiCop2copula
#' @export
copulaFromFamilyIndex <- function(family, par, par2 = 0) {
  constr <- switch(paste("fam", family, sep = ""),
    fam0 = function(par) copula::indepCopula(),
    fam1 = function(par) copula::normalCopula(par[1]),
    fam2 = function(par) copula::tCopula(par[1], df = par[2]),
    fam3 = function(par) copula::claytonCopula(par[1]),
    fam4 = function(par) copula::gumbelCopula(par[1]),
    fam5 = function(par) copula::frankCopula(par[1]),
    fam6 = function(par) joeBiCopula(par[1]),
    fam7 = BB1Copula,
    fam8 = BB6Copula,
    fam9 = BB7Copula,
    fam10 = BB8Copula,
    fam13 = function(par) surClaytonCopula(par[1]),
    fam14 = function(par) surGumbelCopula(par[1]),
    fam16 = function(par) surJoeBiCopula(par[1]),
    fam17 = surBB1Copula,
    fam18 = surBB6Copula,
    fam19 = surBB7Copula,
    fam20 = surBB8Copula,
    fam23 = function(par) r90ClaytonCopula(par[1]),
    fam24 = function(par) r90GumbelCopula(par[1]),
    fam26 = function(par) r90JoeBiCopula(par[1]),
    fam27 = r90BB1Copula,
    fam28 = r90BB6Copula,
    fam29 = r90BB7Copula,
    fam30 = r90BB8Copula,
    fam33 = function(par) r270ClaytonCopula(par[1]),
    fam34 = function(par) r270GumbelCopula(par[1]),
    fam36 = function(par) r270JoeBiCopula(par[1]),
    fam37 = r270BB1Copula,
    fam38 = r270BB6Copula,
    fam39 = r270BB7Copula,
    fam40 = r270BB8Copula,
    fam104 = tawnT1Copula,
    fam114 = surTawnT1Copula,
    fam124 = r90TawnT1Copula,
    fam134 = r270TawnT1Copula,
    fam204 = tawnT2Copula,
    fam214 = surTawnT2Copula,
    fam224 = r90TawnT2Copula,
    fam234 = r270TawnT2Copula
  )
  constr(c(par, par2))
}

copula2BiCop <- function(copula) {
  family <- copula@family
  pars <- copula@parameters
  if (length(pars) == 1) {
    pars <- c(pars, 0)
  }
  if (is.na(pars[2])) {
    pars[2] <- 0
  }

  VineCopula::BiCop(family, pars[1], pars[2], check.pars = FALSE)
}

BiCopPDF.copula <- function(u, copula, log = FALSE, ...) {
  p <- VineCopula::BiCopPDF(u[, 1], u[, 2], copula2BiCop(copula), check = FALSE)
  if (log) {
    p <- log(p)
  }
  p
}

BiCopCDF.copula <- function(u, copula, log = FALSE, ...) {
  VineCopula::BiCopCDF(u[, 1], u[, 2], copula2BiCop(copula), check = FALSE)
}

BiCopHfunc1.copula <- function(u, copula) {
  VineCopula::BiCopHfunc1(u[, 1], u[, 2], copula2BiCop(copula), check = FALSE)
}

BiCopHfunc2.copula <- function(u, copula) {
  VineCopula::BiCopHfunc2(u[, 1], u[, 2], copula2BiCop(copula), check = FALSE)
}

BiCopSim.copula <- function(n, copula) {
  VineCopula::BiCopSim(n, copula2BiCop(copula))
}

BiCopPar2Tau.copula <- function(copula) {
  bc <- copula2BiCop(copula)
  VineCopula::BiCopPar2Tau(bc$family, bc$par, bc$par2)
}

BiCopTau2Par.copula <- function(copula, tau) {
  VineCopula::BiCopTau2Par(copula@family, tau)
}

BiCopPar2TailDep.copula <- function(copula) {
  bc <- copula2BiCop(copula)
  unlist(VineCopula::BiCopPar2TailDep(bc$family, bc$par, bc$par2))
}
