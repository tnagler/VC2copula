#' Survival and rotated Clayton copula models
#'
#' A class representing rotated versions of the Clayton copula family
#' (survival, 90 and 270 degree rotated).
#'
#'
#' @name surClaytonCopula-class
#' @aliases surClaytonCopula-class dduCopula,matrix,surClaytonCopula-method
#' dduCopula,numeric,surClaytonCopula-method
#' ddvCopula,matrix,surClaytonCopula-method
#' ddvCopula,numeric,surClaytonCopula-method r90ClaytonCopula-class
#' dduCopula,matrix,r90ClaytonCopula-method
#' dduCopula,numeric,r90ClaytonCopula-method
#' ddvCopula,matrix,r90ClaytonCopula-method
#' ddvCopula,numeric,r90ClaytonCopula-method r270ClaytonCopula-class
#' dduCopula,matrix,r270ClaytonCopula-method
#' dduCopula,numeric,r270ClaytonCopula-method
#' ddvCopula,matrix,r270ClaytonCopula-method
#' ddvCopula,numeric,r270ClaytonCopula-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' `new("surClaytonCopula", ...)`, `new("r90ClaytonCopula", ...)` and
#' `new("r270ClaytonCopula", ...)` or by the function
#' [surClaytonCopula()], [r90ClaytonCopula()] and
#' [r270ClaytonCopula()] respectively.
#' @seealso [VineCopula-package()]
#' @keywords classes
#' @examples
#'
#' library(copula)
#'
#' persp(surClaytonCopula(.5), dCopula, zlim = c(0, 10))
#' persp(r90ClaytonCopula(-.5), dCopula, zlim = c(0, 10))
#' persp(r270ClaytonCopula(-.5), dCopula, zlim = c(0, 10))
NULL
generateClass("surClaytonCopula")
generateClass("r90ClaytonCopula")
generateClass("r270ClaytonCopula")

#' Constructors for survival and rotated Clayton Copulas
#'
#' These are wrappers to functions from [VineCopula-package()]
#'
#'
#' @aliases surClaytonCopula r90ClaytonCopula r270ClaytonCopula
#' @param param A single parameter defining the Copula.
#' @return An object of class [surClaytonCopula-class],
#' [r90ClaytonCopula-class] or
#' [r270ClaytonCopula-class] respectively.
#' @keywords copula
#' @examples
#'
#' library(copula)
#'
#' persp(surClaytonCopula(1.5), dCopula, zlim = c(0, 10))
#' persp(r90ClaytonCopula(-1.5), dCopula, zlim = c(0, 10))
#' persp(r270ClaytonCopula(-1.5), dCopula, zlim = c(0, 10))
#' @export
surClaytonCopula <- function(param = 1) {
  new("surClaytonCopula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("theta"),
    param.lowbnd = 0,
    param.upbnd = Inf,
    family = 13,
    fullname = "Survival Clayton copula family. Number 13 in VineCopula."
  )
}

#' @export
#' @rdname surClaytonCopula
r90ClaytonCopula <- function(param = -1) {
  new("r90ClaytonCopula",
    dimension = as.integer(2),
    parameters = param,
    param.names = "theta",
    param.lowbnd = -Inf,
    param.upbnd = 0,
    family = 23,
    fullname = "90 deg rotated Clayton copula family. Number 23 in VineCopula."
  )
}

#' @export
#' @rdname surClaytonCopula
r270ClaytonCopula <- function(param = -1) {
  new("r270ClaytonCopula",
    dimension = as.integer(2),
    parameters = param,
    param.names = "theta",
    param.lowbnd = -Inf,
    param.upbnd = 0,
    family = 33,
    fullname = "270 deg rotated Clayton copula family. Number 33 in VineCopula."
  )
}
