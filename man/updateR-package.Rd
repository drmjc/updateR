\docType{package}
\name{updateR-package}
\alias{updateR-package}
\title{Package development toolkit.}
\description{
  A collection of functions that enable R package
  maintainers to build and update packages.
}
\details{
  \code{\link{updateR}} runs \code{roxygen2}, \code{R CMD
  CHECK}, \code{R CMD BUILD} and \code{R CMD INSTALL} to
  generate polished R packages. Execution stops upon
  errors, and all messages are hidden unless there are
  errors.

  Tools for refreshing a currently loaded library are
  included (\code{\link{relibrary}}).

  \tabular{ll}{ Package: \tab updateR\cr Type: \tab
  Package\cr Version: \tab 1.1.3\cr Date: \tab
  2012-07-31\cr License: \tab GPL\cr LazyLoad: \tab yes\cr
  }
}
\author{
  Mark Cowley Maintainer: Mark Cowley
  \email{m.cowley@garvan.org.au} Acknowledgements: Henrik
  Bengtsson for writing the original
  \code{\link{relibrary}} command. Hadley Wickham for
  testthat & devtools. Hadley Wickham + Peter Danenberg +
  Yihui Xie for roxygen2
}
\seealso{
  \code{\link{updateR}},
  \code{\link[R2roxygen]{mjc2roxygen}},
  \code{\link[updateR]{which.package}},
  \code{\link{get.function.dependencies}}
}
\keyword{package}

