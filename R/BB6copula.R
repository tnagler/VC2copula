#' BB6 copula models
#'
#' Wrapper classes representing the BB6, survival BB6, 90 degree and 270 degree
#' rotated BB6 copula families (Joe 1997) from [VineCopula-package()].
#'
#' @name BB6Copula-class
#' @aliases BB6Copula-class dduCopula,numeric,BB6Copula-method
#'   ddvCopula,numeric,BB6Copula-method dduCopula,matrix,BB6Copula-method
#'   ddvCopula,matrix,BB6Copula-method getKendallDistr,BB6Copula-method
#'   kendallDistribution,BB6Copula-method surBB6Copula-class
#'   dduCopula,numeric,surBB6Copula-method ddvCopula,numeric,surBB6Copula-method
#'   dduCopula,matrix,surBB6Copula-method ddvCopula,matrix,surBB6Copula-method
#'   r90BB6Copula-class dduCopula,numeric,r90BB6Copula-method
#'   ddvCopula,numeric,r90BB6Copula-method dduCopula,matrix,r90BB6Copula-method
#'   ddvCopula,matrix,r90BB6Copula-method r270BB6Copula-class
#'   dduCopula,numeric,r270BB6Copula-method
#'   ddvCopula,numeric,r270BB6Copula-method
#'   dduCopula,matrix,r270BB6Copula-method ddvCopula,matrix,r270BB6Copula-method
#' @docType class
#' @section Objects from the Classes: Objects can be created by calls of the
#'   form `new("BB6Copula", ...)`, `new("surBB6Copula", ...)`,
#'   `new("r90BB6Copula", ...)` and `new("r270BB6Copula", ...)` or by the
#'   functions [BB6Copula()], [surBB6Copula()], [r90BB6Copula()] and
#'   [r270BB6Copula()].
#' @seealso See also [BB6Copula-class], [BB7Copula-class], [BB8Copula-class] and
#'   [joeCopula-class] for further wrapper classes to the
#'   [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#'   Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @keywords classes
#' @examples
#'
#' showClass("BB6Copula")
NULL
generateClass("BB6Copula")
generateClass("surBB6Copula")
generateClass("r90BB6Copula")
generateClass("r270BB6Copula")

#' Constructors for BB6 copulas
#'
#' Constructs an object of the [BB6Copula-class] (survival `sur`, 90 degree
#' rotated `r90` and 270 degree rotated `r270`) family for given parameters.
#'
#'
#' @aliases BB6Copula surBB6Copula r90BB6Copula r270BB6Copula
#' @param param The parameter `param` defines the copula through `theta` and
#'   `delta`.
#' @return One of the respective BB6 copula classes ([BB6Copula-class],
#'   [surBB6Copula-class], [r90BB6Copula-class], [r270BB6Copula-class]).
#' @seealso See also [BB6Copula()], [BB7Copula()], [BB8Copula()] and
#'   [joeCopula()] for further wrapper functions to the [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#'   Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @examples
#'
#' library(copula)
#'
#' persp(BB6Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
#' persp(surBB6Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
#' persp(r90BB6Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
#' persp(r270BB6Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
#' @export
BB6Copula <- function(param = c(1, 1)) {
  new("BB6Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(1, 1),
    param.upbnd = c(Inf, Inf),
    family = 8,
    fullname = "BB6 copula family. Number 8 in VineCopula."
  )
}

#' @export
#' @rdname BB6Copula
surBB6Copula <- function(param = c(1, 1)) {
  new("surBB6Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(1, 1),
    param.upbnd = c(Inf, Inf),
    family = 18,
    fullname = "Survival BB6 copula family. Number 18 in VineCopula."
  )
}

#' @export
#' @rdname BB6Copula
r90BB6Copula <- function(param = c(-1, -1)) {
  new("r90BB6Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(-Inf, -Inf),
    param.upbnd = c(-1, -1),
    family = 28,
    fullname = "90 deg rotated BB6 copula family. Number 28 in VineCopula."
  )
}

#' @export
#' @rdname BB6Copula
r270BB6Copula <- function(param = c(-1, -1)) {
  new("r270BB6Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta", "delta"),
    param.lowbnd = c(-Inf, -Inf),
    param.upbnd = c(-1, -1),
    family = 38,
    fullname = "270 deg rotated BB6 copula family. Number 38 in VineCopula."
  )
}
