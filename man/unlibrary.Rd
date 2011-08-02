\name{unlibrary}
\alias{unlibrary}
\title{Unload a library.}
\usage{unlibrary(package, character.only=FALSE, warn.conflicts=TRUE, ...)
}
\description{Unload a library.
Unload a library which has already been \code{load}ed. Adapted from \code{\link{relibrary}}}
\author{Mark Cowley, based on \code{\link{relibrary}} from Henrik Bengtsson, \email{henrikb@braju.com},
\url{http://www.braju.com/R/}}
\seealso{See \code{\link[base]{library}} \code{\link{relibrary}}.}
\value{nothing.}
\arguments{\item{package}{Name or character string giving the name of a package.}
\item{character.only}{A logical indicating whether \sQuote{package} can be
assumed to be character strings. Default value is \code{FALSE}.}
\item{warn.conflicts}{If \code{TRUE}, warnings are printed about conflicts
from reattaching of the package, unless that package contains an object
\code{.conflicts.OK}. Default value is \code{FALSE}.}
\item{\dots}{Currently ignored.}
}
\examples{\dontrun{
unlibrary(updateR)
}}

