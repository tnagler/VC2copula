#' BB8 copula models
#'
#' Wrapper classes representing the BB8, survival BB8, 90 degree and 270 degree
#' rotated BB8 copula families (Joe 1997) from [VineCopula-package()].
#'
#' @name BB8Copula-class
#' @aliases BB8Copula-class dduCopula,numeric,BB8Copula-method
#'   ddvCopula,numeric,BB8Copula-method dduCopula,matrix,BB8Copula-method
#'   ddvCopula,matrix,BB8Copula-method getKendallDistr,BB8Copula-method
#'   kendallDistribution,BB8Copula-method surBB8Copula-class
#'   dduCopula,numeric,surBB8Copula-method ddvCopula,numeric,surBB8Copula-method
#'   dduCopula,matrix,surBB8Copula-method ddvCopula,matrix,surBB8Copula-method
#'   r90BB8Copula-class dduCopula,numeric,r90BB8Copula-method
#'   ddvCopula,numeric,r90BB8Copula-method dduCopula,matrix,r90BB8Copula-method
#'   ddvCopula,matrix,r90BB8Copula-method r270BB8Copula-class
#'   dduCopula,numeric,r270BB8Copula-method
#'   ddvCopula,numeric,r270BB8Copula-method
#'   dduCopula,matrix,r270BB8Copula-method ddvCopula,matrix,r270BB8Copula-method
#' @docType class
#' @section Objects from the Classes: Objects can be created by calls of the
#'   form `new("BB8Copula", ...)`, `new("surBB8Copula", ...)`,
#'   `new("r90BB8Copula", ...)` and `new("r270BB8Copula", ...)` or by the
#'   functions [BB8Copula()], [surBB8Copula()], [r90BB8Copula()] and
#'   [r270BB8Copula()].
#' @seealso See also [BB8Copula-class], [BB8Copula-class], [BB8Copula-class] and
#'   [joeCopula-class] for further wrapper classes to the
#'   [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#'   Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @keywords classes
#' @examples
#'
#' showClass("BB8Copula")
NULL
generateClass("BB8Copula")
generateClass("surBB8Copula")
generateClass("r90BB8Copula")
generateClass("r270BB8Copula")

#' Constructors for BB8 copulas
#'
#' Constructs an object of the [BB8Copula-class] (survival
#' `sur`, 90 degree rotated `r90` and 270 degree rotated `r270`)
#' family for given parameters.
#'
#'
#' @aliases BB8Copula surBB8Copula r90BB8Copula r270BB8Copula
#' @param param The parameter `param` defines the copula through
#' `theta` and `delta`.
#' @return One of the respective BB8 copula classes
#' ([BB8Copula-class], [surBB8Copula-class],
#' [r90BB8Copula-class], [r270BB8Copula-class]).
#' @seealso See also [BB6Copula()], [BB7Copula()],
#' [BB8Copula()] and [joeCopula()] for further wrapper
#' functions to the [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#' Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @examples
#'
#' library(copula)
#'
#' persp(BB8Copula(c(2, 0.9)), dCopula, zlim = c(0, 10))
#' persp(surBB8Copula(c(2, 0.9)), dCopula, zlim = c(0, 10))
#' persp(r90BB8Copula(c(-2, -0.9)), dCopula, zlim = c(0, 10))
#' persp(r270BB8Copula(c(-2, -0.9)), dCopula, zlim = c(0, 10))
#' @export
BB8Copula <- function(param = c(1, 1)) {
  new("BB8Copula",
      dimension = as.integer(2),
      parameters = param,
      param.names = c("theta", "delta"),
      param.lowbnd = c(1, 0),
      param.upbnd = c(Inf, 1),
      family = 10,
      fullname = "BB8 copula family. Number 10 in VineCopula."
  )
}

#' @export
#' @rdname BB8Copula
surBB8Copula <- function(param = c(1, 1)) {
  new("surBB8Copula",
      dimension = as.integer(2),
      parameters = param,
      param.names = c("theta", "delta"),
      param.lowbnd = c(1, 0),
      param.upbnd = c(Inf, 1),
      family = 20,
      fullname = "Survival BB8 copula family. Number 20 in VineCopula."
  )
}

#' @export
#' @rdname BB8Copula
r90BB8Copula <- function(param = c(-1, -1)) {
  new("r90BB8Copula",
      dimension = as.integer(2),
      parameters = param,
      param.names = c("theta", "delta"),
      param.lowbnd = c(-Inf, -1),
      param.upbnd = c(-1, 0),
      family = 30,
      fullname = "90 deg rotated BB8 copula family. Number 30 in VineCopula."
  )
}

#' @export
#' @rdname BB8Copula
r270BB8Copula <- function(param = c(-1, -1)) {
  new(
    "r270BB8Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(-Inf, -1),
    param.upbnd = c(-1, 0),
    family = 40,
    fullname = "270 deg rotated BB8 copula family. Number 40 in VineCopula."
  )
}
