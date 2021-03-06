% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BB1copula.R
\name{BB1Copula}
\alias{BB1Copula}
\alias{surBB1Copula}
\alias{r90BB1Copula}
\alias{r270BB1Copula}
\title{Constructors for BB1 copulas}
\usage{
BB1Copula(param = c(1, 1))

surBB1Copula(param = c(1, 1))

r90BB1Copula(param = c(-1, -1))

r270BB1Copula(param = c(-1, -1))
}
\arguments{
\item{param}{The parameter \code{param} defines the copula through
\code{theta} and \code{delta}.}
}
\value{
One of the respective BB1 copula classes
(\linkS4class{BB1Copula}, \linkS4class{surBB1Copula},
\linkS4class{r90BB1Copula}, \linkS4class{r270BB1Copula}).
}
\description{
Constructs an object of the \linkS4class{BB1Copula} (survival
\code{sur}, 90 degree rotated \code{r90} and 270 degree rotated \code{r270})
family for given parameters.
}
\examples{

library(copula)

persp(BB1Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
persp(surBB1Copula(c(1, 1.5)), dCopula, zlim = c(0, 10))
persp(r90BB1Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
persp(r270BB1Copula(c(-1, -1.5)), dCopula, zlim = c(0, 10))
}
\references{
Joe, H., (1997). Multivariate Models and Dependence Concepts.
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
}
\seealso{
See also \code{\link[=BB6Copula]{BB6Copula()}}, \code{\link[=BB7Copula]{BB7Copula()}},
\code{\link[=BB8Copula]{BB8Copula()}} and \code{\link[=joeCopula]{joeCopula()}} for further wrapper
functions to the \code{\link[=VineCopula-package]{VineCopula-package()}}.
}
\keyword{copula}
\keyword{distribution}
