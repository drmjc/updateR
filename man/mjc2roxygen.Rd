\name{mjc2roxygen}
\alias{mjc2roxygen}
\title{Convert all the comment headers into roxygen headers...}
\usage{mjc2roxygen(pkg, nomatch, usage=FALSE)
}
\description{Convert all the comment headers into roxygen headers}
\details{This function takes a package root directory, parses all its R
files under the R directory and update the corresponding R
source code by inserting roxygen documentation in to the R
scripts.

This is my version of \code{\link[Rd2roxygen]{Rd2roxygen}}, and 90\\% of it is
from Rd2roxygen. It parses all functions, within all source files in a package,
and inserts roxygenized versions of those same comments.}
\note{set verbatim{options(roxygen.comment = "##' ")} to signify that these
roxygen comments are auto-generated, and use \verbatim{"#' "} for 
manually generated comments.}
\arguments{\item{pkg}{the root directory of the package}
\item{nomatch}{PROBABLY HAS NO EFFECT IN MY CODE
the file name (base name only) to use when an object in the
Rd file is not found in any R source files (typically this
happens to the data documentation); if not specified, the
default will be `pkg'-package.R}
\item{usage}{logical: whether to include the usage section in the output
\code{roxygen} can auto-generate these.}
}
\examples{# not run
# mjc2roxygen("~/src/R/mjcdev")}

