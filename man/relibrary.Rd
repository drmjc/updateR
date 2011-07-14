\name{relibrary}
\alias{relibrary}
\title{Reloads a package...}
\usage{relibrary(package, character.only=FALSE, warn.conflicts=TRUE, unload=TRUE, force=FALSE, ...)
}
\description{Reloads a package
Reloads a package. This function works exactly the same as
\code{\link[base]{library}}, \emph{except} that it is reloads the package if
it already loaded. This is useful for developers. For more information see
\code{\link[base]{library}}.}
\details{While relibrary is reloading a package the option \code{relibrary} will be
set to name of the package currently reloaded. This can be useful if the
package to be reloaded would like save away data until it is loaded again.

As of R 2.12, (and perhaps before) reloading of documentation files doesn't
appear to be working. If you change doc files, then for the time being, the
whole R session needs restarting. Relibrary does still work OK for updating
source code changes.}
\note{Currently, if the roxygen comments, or Rd files change, then relibrary is 
unable to update the man files if the package has already been loaded.
The following error is produced when a man page is requested:
\code{Error in tools:::fetchRdDB(RdDB, basename(file)) : 
internal error -3 in R_decompress1}}
\references{\url{https://stat.ethz.ch/pipermail/r-help/2002-January/018006.html}}
\author{Henrik Bengtsson, \email{henrikb@braju.com},
\url{http://www.braju.com/R/}, updated by Mark Cowley.}
\seealso{See \code{\link[base]{library}}.}
\arguments{\item{package}{Name or character string giving the name of a package.}
\item{character.only}{A logical indicating whether \code{package} can be
assumed to be character strings. Default value is \code{FALSE}.}
\item{warn.conflicts}{If \code{TRUE}, warnings are printed about conflicts
from reattaching of the package, unless that package contains an object
\code{.conflicts.OK}. Default value is \code{FALSE}.}
\item{force}{logical: See \code{\link{detach}}.}
\item{...}{Any other arguments that \code{\link[base]{library}} accepts.}
}
