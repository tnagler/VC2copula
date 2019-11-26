#' BB7 copula models
#'
#' Wrapper classes representing the BB7, survival BB7, 90 degree and 270 degree
#' rotated BB7 copula families (Joe 1997) from [VineCopula-package()].
#'
#' @name BB7Copula-class
#' @aliases BB7Copula-class dduCopula,numeric,BB7Copula-method
#'   ddvCopula,numeric,BB7Copula-method dduCopula,matrix,BB7Copula-method
#'   ddvCopula,matrix,BB7Copula-method getKendallDistr,BB7Copula-method
#'   kendallDistribution,BB7Copula-method surBB7Copula-class
#'   dduCopula,numeric,surBB7Copula-method ddvCopula,numeric,surBB7Copula-method
#'   dduCopula,matrix,surBB7Copula-method ddvCopula,matrix,surBB7Copula-method
#'   r90BB7Copula-class dduCopula,numeric,r90BB7Copula-method
#'   ddvCopula,numeric,r90BB7Copula-method dduCopula,matrix,r90BB7Copula-method
#'   ddvCopula,matrix,r90BB7Copula-method r270BB7Copula-class
#'   dduCopula,numeric,r270BB7Copula-method
#'   ddvCopula,numeric,r270BB7Copula-method
#'   dduCopula,matrix,r270BB7Copula-method ddvCopula,matrix,r270BB7Copula-method
#' @docType class
#' @section Objects from the Classes: Objects can be created by calls of the
#'   form `new("BB7Copula", ...)`, `new("surBB7Copula", ...)`,
#'   `new("r90BB7Copula", ...)` and `new("r270BB7Copula", ...)` or by the
#'   functions [BB7Copula()], [surBB7Copula()], [r90BB7Copula()] and
#'   [r270BB7Copula()].
#' @seealso See also [BB7Copula-class], [BB7Copula-class], [BB8Copula-class] and
#'   [joeCopula-class] for further wrapper classes to the
#'   [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#'   Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @keywords classes
#' @examples
#'
#' showClass("BB7Copula")
NULL
generateClass("BB7Copula")
generateClass("surBB7Copula")
generateClass("r90BB7Copula")
generateClass("r270BB7Copula")


#' Constructors for BB7 copulas
#'
#' Constructs an object of the [BB7Copula-class] (survival
#' `sur`, 90 degree rotated `r90` and 270 degree rotated `r270`)
#' family for given parameters.
#'
#'
#' @aliases BB7Copula surBB7Copula r90BB7Copula r270BB7Copula
#' @param param The parameter `param` defines the copula through
#' `theta` and `delta`.
#' @return One of the respective BB7 copula classes
#' ([BB7Copula-class], [surBB7Copula-class],
#' [r90BB7Copula-class], [r270BB7Copula-class]).
#' @seealso See also [BB6Copula()], [BB7Copula()],
#' [BB8Copula()] and [joeCopula()] for further wrapper
#' functions to the [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#' Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @examples
#'
#' library(copula)
#'
#' persp(BB7Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
#' persp(surBB7Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
#' persp(r90BB7Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
#' persp(r270BB7Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
#' @export
BB7Copula <- function(param = c(1, 1)) {
  new("BB7Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(1, 0),
    param.upbnd = c(Inf, Inf),
    family = 9,
    fullname = "BB7 copula family. Number 9 in VineCopula."
  )
}

#' @export
#' @rdname BB7Copula
surBB7Copula <- function(param = c(1, 1)) {
  new("surBB7Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(1, 0),
    param.upbnd = c(Inf, Inf),
    family = 19,
    fullname = "Survival BB7 copula family. Number 19 in VineCopula."
  )
}

#' @export
#' @rdname BB7Copula
r90BB7Copula <- function(param = c(-1, -1)) {
  new("r90BB7Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(-Inf, -Inf),
    param.upbnd = c(-1, 0),
    family = 29,
    fullname = "90 deg rotated BB7 copula family. Number 29 in VineCopula."
  )
}

#' @export
#' @rdname BB7Copula
r270BB7Copula <- function(param = c(-1, -1)) {
  new("r270BB7Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(-Inf, -Inf),
    param.upbnd = c(-1, 0),
    family = 39,
    fullname = "270 deg rotated BB7 copula family. Number 39 in VineCopula."
  )
}
