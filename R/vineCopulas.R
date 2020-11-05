#' Class \code{"vineCopula"}
#'
#' A class representing vine copulas in a object oriented implementations. Many
#' functions go back to the package \code{\link{VineCopula-package}}
#'
#'
#' @name vineCopula-class
#' @aliases vineCopula-class fitCopula,vineCopula-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("vineCopula", ...)} or through the function
#' \code{\link{vineCopula}}.
#' @author Benedikt Graeler
#' @seealso \code{\link{RVineMatrix}} from package
#' \code{\link{VineCopula-package}}
#' @references Aas, K., C. Czado, A. Frigessi, and H. Bakken (2009).
#' Pair-copula constructions of multiple dependence Insurance: Mathematics and
#' Economics 44 (2), 182-198.
#' @keywords classes
#' @examples
#'
#' showClass("vineCopula")
#' @exportClass vineCopula
NULL

validVineCopula <- function(object) {
  dim <- object@dimension
  if (dim <= 2) {
    return("Number of dimension too small (>2).")
  }
  if (length(object@copulas) != (dim * (dim - 1) / 2)) {
    return("Number of provided copulas does not match given dimension.")
  }
  if (!any(sapply(object@copulas, function(x) is(x, "copula") | is(x, "indepCopula")))) {
    return("Not all provided copulas are indeed copulas.")
  }
  p.n <- length(object@parameters)
  if (p.n != length(object@param.upbnd)) {
    return("Parameter and upper bound have non-equal length.")
  }
  if (p.n != length(object@param.lowbnd)) {
    return("Parameter and lower bound have non-equal length.")
  }
  if (p.n != length(object@param.names)) {
    return("Parameter and parameter names have non-equal length.")
  }
  return(TRUE)
}

setOldClass("RVineMatrix")

setClass("vineCopula",
         representation = representation(
           copulas = "list",
           dimension = "integer",
           RVM = "RVineMatrix"
         ),
         prototype = prototype(RVM = structure(list(), class = "RVineMatrix")),
         validity = validVineCopula,
         contains = list("copula")
)

# constructor


#' Constructor of the Class \code{\linkS4class{vineCopula}}.
#'
#' Constructs an instance of the \code{\linkS4class{vineCopula}} class.
#'
#'
#' @param RVM An object of class \code{RVineMatrix} generated from
#' \code{\link{RVineMatrix}} in the package \code{\link{VineCopula-package}} or
#' an integer (e.g. \code{4L}) defining the dimension (an independent Gaussian
#' C-vine of this dimension will be constructed).
#' @param type A predefined type if only the dimension is provided and ignored
#' otherwise, the default is a canonical vine
#' @return An instance of the \code{\linkS4class{vineCopula}} class.
#' @author Benedikt Graeler
#' @references Aas, K., C. Czado, A. Frigessi, and H. Bakken (2009).
#' Pair-copula constructions of multiple dependence Insurance: Mathematics and
#' Economics 44 (2), 182-198.
#' @keywords mulitvariate distribution
#' @examples
#'
#' # a C-vine of independent copulas
#' vine <- vineCopula(4L, "CVine")
#'
#' library(copula)
#' library(lattice)
#'
#' cloud(V1 ~ V2 + V3, as.data.frame(rCopula(500, vine)))
#' @export
vineCopula <- function(RVM, type = "CVine") { # RVM <- 4L
  if (is.integer(RVM)) { # assuming a dimension
    stopifnot(type %in% c("CVine", "DVine"))
    d <- RVM
    stopifnot(d > 1)
    dd <- choose(d, 2)
    if (type == "CVine") {
      RVM <- VineCopula::C2RVine(1:d, rep(1, dd), rep(0, dd))
    } else {
      RVM <- VineCopula::D2RVine(1:d, rep(1, dd), rep(0, dd))
    }
  }

  stopifnot(inherits(RVM, "RVineMatrix"))

  ltr <- lower.tri(RVM$Matrix)
  copDef <- cbind(RVM$family[ltr], RVM$par[ltr], RVM$par2[ltr])
  copulas <- rev(apply(copDef, 1, function(x) BiCop2copula(x[1], x[2], x[3])))

  new("vineCopula",
      copulas = copulas,
      dimension = as.integer(nrow(RVM$Matrix)),
      RVM = RVM,
      parameters = unlist(
        sapply(
          copulas,
          function(x) if(is(x, "indepCopula")) 0 else x@parameters
        )
      ),
      param.names = unlist(
        sapply(
          copulas,
          function(x) if(is(x, "indepCopula")) "" else x@param.names
        )
      ),
      param.lowbnd = unlist(
        sapply(
          copulas,
          function(x) if(is(x, "indepCopula")) 0 else x@param.lowbnd
        )
      ),
      param.upbnd = unlist(
        sapply(
          copulas,
          function(x) if(is(x, "indepCopula")) 0 else x@param.upbnd
        )
      ),
      fullname = paste("RVine copula family.")
  )
}



showVineCopula <- function(object) {
  dim <- object@dimension
  cat(object@fullname, "\n")
  cat("Dimension: ", dim, "\n")
  cat("Represented by the following", dim * (dim - 1) / 2, "copulas:\n")
  for (i in 1:length(object@copulas)) {
    cat(
      "  ", class(object@copulas[[i]]), "with parameter(s)",
      object@copulas[[i]]@parameters, "\n"
    )
  }
}

setMethod("show", signature("vineCopula"), showVineCopula)


RVinePDF.copula <- function(u, copula, log = FALSE) {
  ll <- VineCopula::RVineLogLik(u, copula@RVM, separate = TRUE)$loglik
  if (log)  ll else exp(ll)
}

setMethod(
  "dCopula", signature("numeric", "vineCopula"),
  function(u, copula, log, ...)
    RVinePDF.copula(matrix(u, ncol = copula@dimension), copula, log, ...)
)
setMethod(
  "dCopula", signature("matrix", "vineCopula"),
  RVinePDF.copula
)
setMethod(
  "dCopula", signature("data.frame", "vineCopula"),
  function(u, copula, log, ...)
    RVinePDF.copula(as.matrix(u), copula, log, ...)
)

setMethod("rCopula",
          signature("numeric", "vineCopula"),
          function(n, copula) VineCopula::RVineSim(n, copula@RVM))

# fitCopula
fitVineCop <- function(copula, data,
                       method = list(StructureSelect = FALSE,
                                     indeptest = FALSE,
                                     familyset = NA,
                                     indeptest = FALSE), ...) {
  stopifnot(copula@dimension == ncol(data))

  defaults <- list(StructureSelect = FALSE,
                   indeptest = FALSE,
                   familyset = NA,
                   indeptest = FALSE)
  method <- modifyList(defaults, method)

  if (method[["StructureSelect"]]) {
    RVM <- VineCopula::RVineStructureSelect(data,
                                            familyset = method[["familyset"]],
                                            indeptest =  method[["indeptest"]])
  } else {
    RVM <- VineCopula::RVineCopSelect(data,
                                      familyset = method[["familyset"]],
                                      Matrix = copula@RVM$Matrix,
                                      indeptest =  method[["indeptest"]])
  }
  vineCop <- vineCopula(RVM)

  new("fitCopula",
      copula = vineCop,
      estimate = vineCop@parameters,
      var.est = matrix(NA),
      loglik = VineCopula::RVineLogLik(data, RVM)$loglik,
      nsample = nrow(data),
      method = paste(names(method), method, sep = "=", collapse = ", "),
      call = match.call(),
      fitting.stats = list(convergence = as.integer(NA))
  )
}

setMethod("fitCopula", signature = signature("vineCopula"), fitVineCop)
