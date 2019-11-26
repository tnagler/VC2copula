#' Tawn copula models (type 1)
#'
#' S4-class representation of the Tawn Copula family of type 1 and rotated
#' versions there of.
#'
#'
#' @name tawnT1Copula-class
#' @aliases tawnT1Copula-class dduCopula,matrix,tawnT1Copula-method
#' dduCopula,numeric,tawnT1Copula-method ddvCopula,matrix,tawnT1Copula-method
#' ddvCopula,numeric,tawnT1Copula-method surTawnT1Copula-class
#' dduCopula,matrix,surTawnT1Copula-method
#' dduCopula,numeric,surTawnT1Copula-method
#' ddvCopula,matrix,surTawnT1Copula-method
#' ddvCopula,numeric,surTawnT1Copula-method r90TawnT1Copula-class
#' dduCopula,matrix,r90TawnT1Copula-method
#' dduCopula,numeric,r90TawnT1Copula-method
#' ddvCopula,matrix,r90TawnT1Copula-method
#' ddvCopula,numeric,r90TawnT1Copula-method r270TawnT1Copula-class
#' dduCopula,matrix,r270TawnT1Copula-method
#' dduCopula,numeric,r270TawnT1Copula-method
#' ddvCopula,matrix,r270TawnT1Copula-method
#' ddvCopula,numeric,r270TawnT1Copula-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' `new("tawnT1Copula", ...)`, or through the explicit constructors
#' [tawnT1Copula()], [surTawnT1Copula()],
#' [r90TawnT1Copula()] and [r270TawnT1Copula()]
#' respectively.
#' @seealso [tawnT2Copula-class] and the package
#' [VineCopula-package()] for implementation details.
#' @keywords classes
#' @examples
#'
#' showClass("tawnT1Copula")
NULL
generateClass("tawnT1Copula")
generateClass("surTawnT1Copula")
generateClass("r90TawnT1Copula")
generateClass("r270TawnT1Copula")

#' Constructor for Tawn copulas (type 1)
#'
#' Constructs an object of the [tawnT1Copula-class] (survival
#' `sur`, 90 degree rotated `r90` and 270 degree rotated `r270`)
#' family for given parameters.
#'
#'
#' @aliases tawnT1Copula surTawnT1Copula r90TawnT1Copula r270TawnT1Copula
#' @param param The parameter `param` defines the copula through
#' `param1` and `param2`.
#' @return One of the Tawn type 1 copula classes
#' ([tawnT1Copula-class], [surTawnT1Copula-class],
#' [r90TawnT1Copula-class],
#' [r270TawnT1Copula-class]).
#' @seealso [tawnT2Copula()] and the package
#' [VineCopula-package()] for implementation details.
#' @keywords distribution copula
#' @examples
#'
#' library(copula)
#'
#' persp(tawnT1Copula(), dCopula, zlim = c(0, 10))
#' persp(surTawnT1Copula(), dCopula, zlim = c(0, 10))
#' persp(r90TawnT1Copula(), dCopula, zlim = c(0, 10))
#' persp(r270TawnT1Copula(), dCopula, zlim = c(0, 10))
#' @export
tawnT1Copula <- function(param = c(2, 0.5)) {
  new("tawnT1Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(1, 0),
    param.upbnd = c(Inf, 1),
    family = 104,
    fullname = "Tawn type 1 copula family. Number 104 in VineCopula."
  )
}

# Pickand's A
# c-code: Tawn2(double* t, int* n, double* par, double* par2, double* par3, double* out)
setMethod("A", signature("tawnT1Copula"), function(copula, w) {
  .C("Tawn2", as.double(w), as.integer(length(w)),
    as.double(copula@parameters[1]), as.double(copula@parameters[2]),
    as.double(1), as.double(rep(0, length(w))),
    PACKAGE = "VineCopula"
  )[[6]]
})

#' @export
#' @rdname tawnT1Copula
surTawnT1Copula <- function(param = c(2, 0.5)) {
  new("surTawnT1Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(1, 0),
    param.upbnd = c(Inf, 1),
    family = 114,
    fullname = "Survival Tawn type 1 copula family. Number 114 in VineCopula."
  )
}

# Pickand's A
# c-code: Tawn2(double* t, int* n, double* par, double* par2, double* par3, double* out)
setMethod("A", signature("surTawnT1Copula"), function(copula, w) {
  u <- -expm1(-1 + w)
  v <- -expm1(-w)

  surA <- .C("Tawn2", as.double(log(v) / log(u * v)), as.integer(length(w)),
    as.double(copula@parameters[1]), as.double(copula@parameters[2]),
    as.double(1), as.double(rep(0, length(w))),
    PACKAGE = "VineCopula"
  )[[6]]
  -log(1 - u + 1 - v - 1 + (u * v)^surA)
  # -log(1-u + 1-v - 1 + VineCopula:::BiCopCDF.copula(cbind(u,v), tawnT1Copula(copula@parameters)))
})


#' @export
#' @rdname tawnT1Copula
r90TawnT1Copula <- function(param = c(-2, 0.5)) {
  new("r90TawnT1Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(-Inf, 0),
    param.upbnd = c(-1, 1),
    family = 124,
    fullname = "90 deg rotated Tawn type 1 copula family. Number 124 in VineCopula."
  )
}

#' @export
#' @rdname tawnT1Copula
r270TawnT1Copula <- function(param = c(-2, 0.5)) {
  new("r270TawnT1Copula",
    dimension = as.integer(2),
    parameters = param,
    param.names = c("param1", "param2"),
    param.lowbnd = c(-Inf, 0),
    param.upbnd = c(-1, 1),
    family = 134,
    fullname = "270 deg rotated Tawn type 1 copula family. Number 134 in VineCopula."
  )
}
