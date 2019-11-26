
#' Survival and rotated Gumbel copula models
#'
#' A class representing rotated versions of the Gumbel copula family
#' (survival, 90 and 270 degree rotated).
#'
#'
#' @name surGumbelCopula-class
#' @aliases surGumbelCopula-class dduCopula,matrix,surGumbelCopula-method
#' dduCopula,numeric,surGumbelCopula-method
#' ddvCopula,matrix,surGumbelCopula-method
#' ddvCopula,numeric,surGumbelCopula-method r90GumbelCopula-class
#' dduCopula,matrix,r90GumbelCopula-method
#' dduCopula,numeric,r90GumbelCopula-method
#' ddvCopula,matrix,r90GumbelCopula-method
#' ddvCopula,numeric,r90GumbelCopula-method r270GumbelCopula-class
#' dduCopula,matrix,r270GumbelCopula-method
#' dduCopula,numeric,r270GumbelCopula-method
#' ddvCopula,matrix,r270GumbelCopula-method
#' ddvCopula,numeric,r270GumbelCopula-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' `new("surGumbelCopula", ...)`, `new("r90GumbelCopula", ...)` and
#' `new("r270GumbelCopula", ...)` or by the function
#' [surGumbelCopula()], [r90GumbelCopula()] and
#' [r270GumbelCopula()] respectively.
#' @seealso [VineCopula-package()]
#' @keywords classes
#' @examples
#'
#' library(copula)
#'
#' persp(surGumbelCopula(5), dCopula, zlim = c(0, 10))
#' persp(r90GumbelCopula(-5), dCopula, zlim = c(0, 10))
#' persp(r270GumbelCopula(-5), dCopula, zlim = c(0, 10))
NULL
generateClass("surGumbelCopula")
generateClass("r90GumbelCopula")
generateClass("r270GumbelCopula")

#' Constructors for survival and rotated Gumbel Copulas
#'
#' These are wrappers to functions from [VineCopula-package()]
#'
#'
#' @aliases surGumbelCopula r90GumbelCopula r270GumbelCopula
#' @param param A single parameter defining the Copula.
#' @return An object of class [surGumbelCopula-class],
#' [r90GumbelCopula-class] or
#' [r270GumbelCopula-class] respectively.
#' @keywords copula
#' @examples
#'
#' library(copula)
#'
#' persp(surGumbelCopula(1.5), dCopula, zlim = c(0, 10))
#' persp(r90GumbelCopula(-1.5), dCopula, zlim = c(0, 10))
#' persp(r270GumbelCopula(-1.5), dCopula, zlim = c(0, 10))
#' @export
surGumbelCopula <- function(param = 1) {
  new("surGumbelCopula",
      dimension = as.integer(2),
      parameters = param,
      param.names = c("theta"),
      param.lowbnd = 1,
      param.upbnd = Inf,
      family = 14,
      fullname = "Survival Gumbel copula family. Number 14 in VineCopula."
  )
}

#' @export
#' @rdname surGumbelCopula
r90GumbelCopula <- function(param = -1) {
  new("r90GumbelCopula",
      dimension = as.integer(2),
      parameters = param,
      param.names = "theta",
      param.lowbnd = -Inf,
      param.upbnd = -1,
      family = 24,
      fullname = "90 deg rotated Gumbel copula family. Number 24 in VineCopula."
  )
}

#' @export
#' @rdname surGumbelCopula
r270GumbelCopula <- function(param = -1) {
  new("r270GumbelCopula",
      dimension = as.integer(2),
      parameters = param,
      param.names = "theta",
      param.lowbnd = -Inf,
      param.upbnd = -1,
      family = 34,
      fullname = "270 deg rotated Gumbel copula family. Number 34 in VineCopula."
  )
}
