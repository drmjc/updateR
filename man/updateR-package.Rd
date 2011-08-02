\name{updateR-package}
\alias{updateR-package}
\alias{updateR-package}
\title{Package development toolkit.}
\description{Package development toolkit.
A collection of functions that enable R package maintainers to build and update packages.}
\details{\code{\link{updateR}} runs \code{roxygen}, \code{R CMD CHECK}, \code{R CMD BUILD} and 
\code{R CMD INSTALL} to generate polished R packages. Execution stops upon errors, and 
all messages are hidden unless there are errors.

Tools for refreshing a currently loaded library are included (\code{\link{relibrary}}).

If you've ever wanted to send someone a function, only to find that you forgot to also
send the function dependencies? \code{\link{get.function.dependencies}} is a method 
which can trawl through all the function dependencies, retrieve the original source 
definitions, complete with header comments & combine them into a single file, suitable
for sharing.

\tabular{ll}{ 
Package: \tab updateR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-07-19\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}}
\alias{updateR-package}
\docType{package}
\author{Mark Cowley
Maintainer: Mark Cowley \email{m.cowley@garvan.org.au}
Acknowledgements: Henrik Bengtsson for writing the original \code{\link{relibrary}} command.}
\seealso{\code{\link{updateR}}, \code{\link{mjc2roxygen}}, \code{\link[updateR]{which.package}}, \code{\link{get.function.dependencies}}}
\keyword{package}

