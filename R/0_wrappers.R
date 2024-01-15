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
  family <- try(copula@family, silent = TRUE)
  if (inherits(family, "try-error")) {
    family <- switch(
      class(copula),
      "indepCopula" = 0,
      "normalCopula" = 1,
      "tCopula" = 2,
      "claytonCopula" = 3,
      "gumbelCopula" = 4,
      "frankCopula" = 5,
      NA
    )
  }
  pars <- if (family != 0) copula@parameters else 0
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

# methods in copula
# iTau
BCiTau <- function(copula, tau) VineCopula::BiCopTau2Par(copula@family, tau)

#' A dedicated method to use the estimation routines from the VineCopula package
#'
#' Bivariate copulas are estimated based on \code{\link[VineCopula]{BiCopEst}} and vine copulas through \code{\link[VineCopula]{RVineStructureSelect}} or \code{\link[VineCopula]{RVineCopSelect}} depending on the \code{method} argument.
#'
#' @param copula an object of the desired copula class
#' @param data a matrix holding the U(0,1) distributed data columns
#' @param method for BIVARIATE copulas either "ml" or "itau" for maximum likelihood estimation or inverse tau estimation (for one parameter families) respectively. See \code{\link[VineCopula]{BiCopEst}} for details. In case of a VINE copulas a list with names entries \code{StructureSelect} (default: FALSE), \code{indeptest} (default: FALSE), \code{familyset} (default: 'NA') and \code{indeptest} (default: FALSE). See \code{\link[VineCopula]{RVineStructureSelect}} or \code{\link[VineCopula]{RVineCopSelect}} for details.
#'
#' @return an object of class \code{\link[copula]{fitCopula}} as in the copula package.
#'
#' @examples
#'
#' u <- rCopula(1000, tawnT1Copula(c(3, 0.5)))
#'
#' fitCopula(tawnT1Copula(), u)
#'
#' @aliases fitCopula
#' @name fitCopula
BCfitCopula <- function(copula, data, method="ml") {
  stopifnot(method %in% c("ml", "itau"))
  if (method == "itau") {
    stopifnot(copula@family %in% c(1,2,3,4,5,6,13,14,16,23,24,26,33,34,36))
  }
  if (method == "ml")
    method <- "mle"

  BCestimate <- VineCopula::BiCopEst(data[,1], data[,2], copula@family,
                                     method = method)

  new("fitCopula",
      copula = BiCop2copula(BCestimate),
      estimate = c(BCestimate$par, ifelse(!is.na(BCestimate$par2),
                                          BCestimate$par2, NULL)),
      var.est = matrix(NA),
      loglik = BCestimate$logLik,
      nsample = BCestimate$nobs,
      method = method,
      call = match.call(),
      fitting.stats = list(AIC = BCestimate$AIC,
                           BIC = BCestimate$BIC,
                           convergence = NA))
}
