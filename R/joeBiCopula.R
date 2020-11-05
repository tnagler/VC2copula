#' Joe copula models
#'
#' Wrapper classes representing the bivariate Joe, survival Joe, 90 degree and
#' 270 degree rotated Joe copula families (Joe 1997) from
#' [VineCopula-package()]. Note that package
#' [copula-package()] provides a class [joeCopula-class]
#' as well.
#'
#'
#' @name joeBiCopula-class
#' @aliases joeBiCopula-class dduCopula,numeric,joeBiCopula-method
#' ddvCopula,numeric,joeBiCopula-method dduCopula,matrix,joeBiCopula-method
#' ddvCopula,matrix,joeBiCopula-method getKendallDistr,joeBiCopula-method
#' kendallDistribution,joeBiCopula-method surJoeBiCopula-class
#' dduCopula,numeric,surJoeBiCopula-method
#' ddvCopula,numeric,surJoeBiCopula-method
#' dduCopula,matrix,surJoeBiCopula-method
#' ddvCopula,matrix,surJoeBiCopula-method r90JoeBiCopula-class
#' dduCopula,numeric,r90JoeBiCopula-method
#' ddvCopula,numeric,r90JoeBiCopula-method
#' dduCopula,matrix,r90JoeBiCopula-method
#' ddvCopula,matrix,r90JoeBiCopula-method r270JoeBiCopula-class
#' dduCopula,numeric,r270JoeBiCopula-method
#' ddvCopula,numeric,r270JoeBiCopula-method
#' dduCopula,matrix,r270JoeBiCopula-method
#' ddvCopula,matrix,r270JoeBiCopula-method
#' @docType class
#' @section Objects from the Classes: Objects can be created by calls of the
#' form `new("joeBiCopula", ...)`, `new("surJoeBiCopula", ...)`,
#' `new("r90JoeBiCopula", ...)` and `new("r270JoeBiCopula", ...)` or
#' by the functions [joeBiCopula()], [surJoeBiCopula()],
#' [r90JoeBiCopula()] and [r270JoeBiCopula()].
#' @seealso See also [BB1Copula-class],
#' [BB6Copula-class], [BB7Copula-class] and
#' [BB8Copula-class] for further wrapper classes to the
#' [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#' Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @keywords classes
#' @examples
#'
#' showClass("surJoeBiCopula")
NULL

#' @exportClass joeBiCopula surJoeBiCopula r90JoeBiCopula r270JoeBiCopula
generateClass("joeBiCopula")
generateClass("surJoeBiCopula")
generateClass("r90JoeBiCopula")
generateClass("r270JoeBiCopula")


#' Constructors for Joe copulas
#'
#' Constructs an object of the (survival `surJoeBiCopula`, 90 degree
#' rotated `r90JoeBiCopula` and 270 degree rotated `r270JoeBiCopula`)
#' family for a given parameter. Note that package [copula-package()]
#' provides a class [joeCopula-class] as well.
#'
#'
#' @aliases joeBiCopula surJoeBiCopula r90JoeBiCopula r270JoeBiCopula
#' @param param The parameter `param` defines the copula through
#' `theta`.
#' @return One of the respective Joe copula classes
#' ([joeBiCopula-class], [surJoeBiCopula-class],
#' [r90JoeBiCopula-class], [r270JoeBiCopula-class]).
#' @seealso See also [BB1Copula()], [BB6Copula()],
#' [BB7Copula()] and [BB8Copula()] for further wrapper
#' functions to the [VineCopula-package()].
#' @references Joe, H., (1997). Multivariate Models and Dependence Concepts.
#' Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
#' @examples
#'
#' library(copula)
#'
#' persp(surJoeBiCopula(1.5), dCopula, zlim = c(0, 10))
#' persp(r90JoeBiCopula(-1.5), dCopula, zlim = c(0, 10))
#' persp(r270JoeBiCopula(-1.5), dCopula, zlim = c(0, 10))
#' @export
joeBiCopula <- function(param = 2) {
  new("joeBiCopula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta"),
    param.lowbnd = 1,
    param.upbnd = Inf,
    family = 6,
    fullname = "Joe copula family. Number 6 in VineCopula."
  )
}


#' @export
#' @rdname joeBiCopula
surJoeBiCopula <- function(param = 2) {
  new("surJoeBiCopula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta"),
    param.lowbnd = 1,
    param.upbnd = Inf,
    family = 16,
    fullname = "Survival Joe copula family. Number 16 in VineCopula."
  )
}

#' @export
#' @rdname joeBiCopula
r90JoeBiCopula <- function(param = -2) {
  new("r90JoeBiCopula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta"),
    param.lowbnd = -Inf,
    param.upbnd = -1,
    family = 26,
    fullname = "90 deg rotated Joe copula family. Number 26 in VineCopula."
  )
}

#' @export
#' @rdname joeBiCopula
r270JoeBiCopula <- function(param = -2) {
  new("r270JoeBiCopula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta"),
    param.lowbnd = -Inf,
    param.upbnd = -1,
    family = 36,
    fullname = "270 deg rotated Joe copula family. Number 36 in VineCopula."
  )
}

# inverse tau
setMethod("iTau", signature = c("joeBiCopula", "ANY"), BCiTau)
setMethod("iTau", signature = c("r90JoeBiCopula", "ANY"), BCiTau)
setMethod("iTau", signature = c("surJoeBiCopula", "ANY"), BCiTau)
setMethod("iTau", signature = c("r270JoeBiCopula", "ANY"), BCiTau)

# fitCopula
setMethod(fitCopula, "joeBiCopula", BCfitCopula)
setMethod(fitCopula, "surJoeBiCopula", BCfitCopula)
setMethod(fitCopula, "r90JoeBiCopula", BCfitCopula)
setMethod(fitCopula, "r270JoeBiCopula", BCfitCopula)
