#' @rdname ddCopula
setMethod("dduCopula", signature("matrix", "normalCopula"), BiCopHfunc1.copula)
#' @rdname ddCopula
setMethod("dduCopula", signature("numeric", "normalCopula"),
          function(u, copula, ...)
            BiCopHfunc1.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("ddvCopula", signature("matrix", "normalCopula"), BiCopHfunc2.copula)
#' @rdname ddCopula
setMethod("ddvCopula", signature("numeric", "normalCopula"),
          function(u, copula, ...)
            BiCopHfunc2.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("dduCopula", signature("matrix", "tCopula"), BiCopHfunc1.copula)
#' @rdname ddCopula
setMethod("dduCopula", signature("numeric", "tCopula"),
          function(u, copula, ...)
            BiCopHfunc1.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("ddvCopula", signature("matrix", "tCopula"), BiCopHfunc2.copula)
#' @rdname ddCopula
setMethod("ddvCopula", signature("numeric", "tCopula"),
          function(u, copula, ...)
            BiCopHfunc2.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("dduCopula", signature("matrix", "gumbelCopula"), BiCopHfunc1.copula)
#' @rdname ddCopula
setMethod("dduCopula", signature("numeric", "gumbelCopula"),
          function(u, copula, ...)
            BiCopHfunc1.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("ddvCopula", signature("matrix", "gumbelCopula"), BiCopHfunc2.copula)
#' @rdname ddCopula
setMethod("ddvCopula", signature("numeric", "gumbelCopula"),
          function(u, copula, ...)
            BiCopHfunc2.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("dduCopula", signature("matrix", "claytonCopula"), BiCopHfunc1.copula)
#' @rdname ddCopula
setMethod("dduCopula", signature("numeric", "claytonCopula"),
          function(u, copula, ...)
            BiCopHfunc1.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("ddvCopula", signature("matrix", "claytonCopula"), BiCopHfunc2.copula)
#' @rdname ddCopula
setMethod("ddvCopula", signature("numeric", "claytonCopula"),
          function(u, copula, ...)
            BiCopHfunc2.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("dduCopula", signature("matrix", "indepCopula"), BiCopHfunc1.copula)
#' @rdname ddCopula
setMethod("dduCopula", signature("numeric", "indepCopula"),
          function(u, copula, ...)
            BiCopHfunc1.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("ddvCopula", signature("matrix", "indepCopula"), BiCopHfunc2.copula)
#' @rdname ddCopula
setMethod("ddvCopula", signature("numeric", "indepCopula"),
          function(u, copula, ...)
            BiCopHfunc2.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("dduCopula", signature("matrix", "frankCopula"), BiCopHfunc1.copula)
#' @rdname ddCopula
setMethod("dduCopula", signature("numeric", "frankCopula"),
          function(u, copula, ...)
            BiCopHfunc1.copula(matrix(u, ncol = copula@dimension), copula))
#' @rdname ddCopula
setMethod("ddvCopula", signature("matrix", "frankCopula"), BiCopHfunc2.copula)
#' @rdname ddCopula
setMethod("ddvCopula", signature("numeric", "frankCopula"),
          function(u, copula, ...)
            BiCopHfunc2.copula(matrix(u, ncol = copula@dimension), copula))
