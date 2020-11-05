#' Tawn copula models (type 2)
#'
#' S4-class representation of the Tawn Copula family of type 2 and rotated
#' versions there of.
#'
#'
#' @name tawnT2Copula-class
#' @aliases tawnT2Copula-class dduCopula,matrix,tawnT2Copula-method
#' dduCopula,numeric,tawnT2Copula-method ddvCopula,matrix,tawnT2Copula-method
#' ddvCopula,numeric,tawnT2Copula-method surTawnT2Copula-class
#' dduCopula,matrix,surTawnT2Copula-method
#' dduCopula,numeric,surTawnT2Copula-method
#' ddvCopula,matrix,surTawnT2Copula-method
#' ddvCopula,numeric,surTawnT2Copula-method r90TawnT2Copula-class
#' dduCopula,matrix,r90TawnT2Copula-method
#' dduCopula,numeric,r90TawnT2Copula-method
#' ddvCopula,matrix,r90TawnT2Copula-method
#' ddvCopula,numeric,r90TawnT2Copula-method r270TawnT2Copula-class
#' dduCopula,matrix,r270TawnT2Copula-method
#' dduCopula,numeric,r270TawnT2Copula-method
#' ddvCopula,matrix,r270TawnT2Copula-method
#' ddvCopula,numeric,r270TawnT2Copula-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' `new("tawnT2Copula", ...)`, or through the explicit constructors
#' [tawnT2Copula()], [surTawnT2Copula()],
#' [r90TawnT2Copula()] and [r270TawnT2Copula()]
#' respectively.
#' @seealso [tawnT2Copula-class] and the package
#' [VineCopula-package()] for implementation details.
#' @keywords classes
#' @examples
#'
#' showClass("tawnT2Copula")
NULL

#' @exportClass tawnT2Copula surTawnT2Copula r90TawnT2Copula r270TawnT2Copula
generateClass("tawnT2Copula")
generateClass("surTawnT2Copula")
generateClass("r90TawnT2Copula")
generateClass("r270TawnT2Copula")

#' Constructor for Tawn copulas (type 2)
#'
#' Constructs an object of the [tawnT2Copula-class] (survival
#' `sur`, 90 degree rotated `r90` and 270 degree rotated `r270`)
#' family for given parameters.
#'
#'
#' @aliases tawnT2Copula surTawnT2Copula r90TawnT2Copula r270TawnT2Copula
#' @param param The parameter `param` defines the copula through
#' `param1` and `param2`.
#' @return One of the Tawn type 2 copula classes
#' ([tawnT2Copula-class], [surTawnT2Copula-class],
#' [r90TawnT2Copula-class],
#' [r270TawnT2Copula-class]).
#' @seealso [tawnT2Copula()] and the package
#' [VineCopula-package()] for implementation details.
#' @keywords distribution copula
#' @examples
#'
#' library(copula)
#'
#' persp(tawnT2Copula(), dCopula, zlim = c(0, 10))
#' persp(surTawnT2Copula(), dCopula, zlim = c(0, 10))
#' persp(r90TawnT2Copula(), dCopula, zlim = c(0, 10))
#' persp(r270TawnT2Copula(), dCopula, zlim = c(0, 10))
#' @export
tawnT2Copula <- function(param = c(2, 0.5)) {
  new("tawnT2Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(1, 0),
    param.upbnd = c(Inf, 1),
    family = 204,
    fullname = "Tawn type 2 copula family. Number 204 in VineCopula."
  )
}

#' @export
#' @rdname tawnT2Copula
surTawnT2Copula <- function(param = c(2, 0.5)) {
  new("surTawnT2Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(1, 0),
    param.upbnd = c(Inf, 1),
    family = 214,
    fullname = "Survival Tawn type 2 copula family. Number 214 in VineCopula."
  )
}


#' @export
#' @rdname tawnT2Copula
r90TawnT2Copula <- function(param = c(-2, 0.5)) {
  new("r90TawnT2Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(-Inf, 0),
    param.upbnd = c(-1, 1),
    family = 224,
    fullname = "90 deg rotated Tawn type 2 copula family. Number 224 in VineCopula."
  )
}

#' @export
#' @rdname tawnT2Copula
r270TawnT2Copula <- function(param = c(-2, 0.5)) {
  new("r270TawnT2Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(-Inf, 0),
    param.upbnd = c(-1, 1),
    family = 234,
    fullname = "270 deg rotated Tawn type 2 copula family. Number 234 in VineCopula."
  )
}

# fitCopula
setMethod(fitCopula, "tawnT2Copula", BCfitCopula)
setMethod(fitCopula, "surTawnT2Copula", BCfitCopula)
setMethod(fitCopula, "r90TawnT2Copula", BCfitCopula)
setMethod(fitCopula, "r270TawnT2Copula", BCfitCopula)
