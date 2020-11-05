#' BB1 copula models
#'
#' Wrapper classes representing the BB1, survival BB1, 90 degree and 270 degree
#' rotated BB1 copula families (Joe 1997) from [VineCopula-package()].
#'
#' @name BB1Copula-class
#' @aliases BB1Copula-class dduCopula,numeric,BB1Copula-method
#'   ddvCopula,numeric,BB1Copula-method dduCopula,matrix,BB1Copula-method
#'   ddvCopula,matrix,BB1Copula-method getKendallDistr,BB1Copula-method
#'   kendallDistribution,BB1Copula-method surBB1Copula-class
#'   dduCopula,numeric,surBB1Copula-method ddvCopula,numeric,surBB1Copula-method
#'   dduCopula,matrix,surBB1Copula-method ddvCopula,matrix,surBB1Copula-method
#'   r90BB1Copula-class dduCopula,numeric,r90BB1Copula-method
#'   ddvCopula,numeric,r90BB1Copula-method dduCopula,matrix,r90BB1Copula-method
#'   ddvCopula,matrix,r90BB1Copula-method r270BB1Copula-class
#'   dduCopula,numeric,r270BB1Copula-method
#'   ddvCopula,numeric,r270BB1Copula-method
#'   dduCopula,matrix,r270BB1Copula-method ddvCopula,matrix,r270BB1Copula-method
#' @docType class
#' @section Objects from the Classes: Objects can be created by calls of the
#'   form `new("BB1Copula", ...)`, `new("surBB1Copula", ...)`,
#'   `new("r90BB1Copula", ...)` and `new("r270BB1Copula", ...)` or by the
#'   functions [BB1Copula()], [surBB1Copula()], [r90BB1Copula()] and
#'   [r270BB1Copula()].
#' @seealso See also [BB6Copula-class], [BB7Copula-class], [BB8Copula-class] and
#'   [joeCopula-class] for further wrapper classes to the
#'   [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#'   Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @keywords classes
#' @examples
#'
#' showClass("BB1Copula")
NULL

#' @exportClass BB1Copula surBB1Copula r90BB1Copula r270BB1Copula
generateClass("BB1Copula")
generateClass("surBB1Copula")
generateClass("r90BB1Copula")
generateClass("r270BB1Copula")


#' Constructors for BB1 copulas
#'
#' Constructs an object of the [BB1Copula-class] (survival
#' `sur`, 90 degree rotated `r90` and 270 degree rotated `r270`)
#' family for given parameters.
#'
#'
#' @aliases BB1Copula surBB1Copula r90BB1Copula r270BB1Copula
#' @param param The parameter `param` defines the copula through
#' `theta` and `delta`.
#' @return One of the respective BB1 copula classes
#' ([BB1Copula-class], [surBB1Copula-class],
#' [r90BB1Copula-class], [r270BB1Copula-class]).
#' @seealso See also [BB6Copula()], [BB7Copula()],
#' [BB8Copula()] and [joeCopula()] for further wrapper
#' functions to the [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#' Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @keywords distribution copula
#' @examples
#'
#' library(copula)
#'
#' persp(BB1Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
#' persp(surBB1Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
#' persp(r90BB1Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
#' persp(r270BB1Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
#' @export
BB1Copula <- function(param = c(1, 1)) {
  new("BB1Copula",
      dimension = as.integer(2),
      parameters = param,
      param.names = c("theta", "delta"),
      param.lowbnd = c(0, 1),
      param.upbnd = c(Inf, Inf),
      family = 7,
      fullname = "BB1 copula family. Number 7 in VineCopula."
  )
}

#' @export
#' @rdname BB1Copula
surBB1Copula <- function(param = c(1, 1)) {
  new("surBB1Copula",
    dimension = as.integer(2), parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(0, 1),
    param.upbnd = c(Inf, Inf),
    family = 17,
    fullname = "Survival BB1 copula family. Number 17 in VineCopula."
  )
}

#' @export
#' @rdname BB1Copula
r90BB1Copula <- function(param = c(-1, -1)) {
  new("r90BB1Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(-Inf, -Inf),
    param.upbnd = c(0, -1),
    family = 27,
    fullname = "90 deg rotated BB1 copula family. Number 27 in VineCopula."
  )
}

#' @export
#' @rdname BB1Copula
r270BB1Copula <- function(param = c(-1, -1)) {
  new("r270BB1Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(-Inf, -Inf),
    param.upbnd = c(0, -1),
    family = 37,
    fullname = "270 deg rotated BB1 copula family. Number 37 in VineCopula."
  )
}

# fitCopula
setMethod(fitCopula, "BB1Copula", BCfitCopula)
setMethod(fitCopula, "surBB1Copula", BCfitCopula)
setMethod(fitCopula, "r90BB1Copula", BCfitCopula)
setMethod(fitCopula, "r270BB1Copula", BCfitCopula)
