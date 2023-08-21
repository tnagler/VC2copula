#' Partial Derivatives of Copulas
#'
#' Similar to [dCopula()] and [pCopula()] the function
#' `dduCopula` evaluates the partial derivative
#' \eqn{\frac{\partial}{\partial u} C(u,v)}{d C(u,v)/du} and the function
#' `ddvCopula` evaluates the partial derivative
#' \eqn{\frac{\partial}{\partial v} C(u,v)}{d C(u,v)/dv} of the provided
#' copula.
#'
#' @name ddCopula
#' @aliases ddCopula dduCopula ddvCopula
#' @param u Pairs of values for which the partial derivative should be
#' evaluated.
#' @param copula The copula object representing the family member of interest.
#' @param \dots additional arguments can be passed on to the underlying
#' functions.
#' @return A vector of the evaluated partial derivatives of the same length as
#' rows in `u`.
#' @keywords partial derivative conditional probabilities
#' @examples
#'
#' library(copula)
#'
#' BB1Cop <- BB1Copula()
#' BB1CopSmpl <- rCopula(100, BB1Cop)
#'
#' # conditional probabilities of a Gaussian copula given u
#' BB1GivenU <- dduCopula(BB1CopSmpl, BB1Cop)
#'
#' # vs. conditional probabilities of a Gaussian copula given v
#' BB1GivenV <- ddvCopula(BB1CopSmpl[, c(2, 1)], BB1Cop)
#'
#' plot(BB1GivenU, BB1GivenV)
#' abline(0, 1)
#'
#' @exportMethod dduCopula ddvCopula
setGeneric("dduCopula", function(u, copula, ...) standardGeneric("dduCopula"))
setGeneric("ddvCopula", function(u, copula, ...) standardGeneric("ddvCopula"))

generateClass <- function(name) {
  checkValidity <- function(object) {
    if (object@dimension != 2)
      return(paste0("Only ", name, "-copulas of dimension 2 are supported."))
    p.n <- length(object@parameters)
    if (p.n != length(object@param.upbnd))
      return("Parameter and upper bound have non-equal length.")
    if (p.n != length(object@param.lowbnd))
      return("Parameter and lower bound have non-equal length.")
    if (p.n != length(object@param.names))
      return("Parameter and parameter names have non-equal length.")
    e <- try(copula2BiCop(object))
    if (inherits(e, "try-error"))
      return(attr(e, "condition")$message)
    TRUE
  }

  setClass(name,
           slots = c(family = "numeric"),
           validity = checkValidity,
           contains = "copula")

  setMethod("dCopula", signature("matrix", name), BiCopPDF.copula)
  setMethod("dCopula", signature("numeric", name),
            function(u, copula, log)
              BiCopPDF.copula(matrix(u, ncol = copula@dimension), copula, log))
  setMethod("pCopula", signature("matrix", name), BiCopCDF.copula)
  setMethod("pCopula", signature("numeric", name),
            function(u, copula, ...)
              BiCopCDF.copula(matrix(u, ncol = copula@dimension), copula))
  setMethod("dduCopula", signature("matrix", name), BiCopHfunc1.copula)
  setMethod("dduCopula", signature("numeric", name),
            function(u, copula, ...)
              BiCopHfunc1.copula(matrix(u, ncol = copula@dimension), copula))
  setMethod("ddvCopula", signature("matrix", name), BiCopHfunc2.copula)
  setMethod("ddvCopula", signature("numeric", name),
            function(u, copula, ...)
              BiCopHfunc2.copula(matrix(u, ncol = copula@dimension), copula))
  setMethod("rCopula", signature("numeric", name), BiCopSim.copula)
  setMethod("tau", signature(name), BiCopPar2Tau.copula)
  setMethod("lambda", signature(name), BiCopPar2TailDep.copula)
  setMethod("A", signature(name),
            function(copula, w)
              -log(BiCopCDF.copula(cbind(exp(w - 1), exp(-w)), copula)))
}
