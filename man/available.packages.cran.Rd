\name{available.packages.cran}
\alias{available.packages.cran}
\title{Download a current list of available CRAN packages}
\usage{
  available.packages.cran(prefix = getOption("repos"))
}
\arguments{
  \item{prefix}{the URL prefix, eg:
  \url{http://mirror.aarnet.edu.au/pub/CRAN}}
}
\value{
  Same as the output from
  \code{\link[utils]{available.packages}}, but only listing
  packages from CRAN: A \code{matrix} with one row per
  package, row names the package names and column names
  \dQuote{Package}, \dQuote{Version}, \dQuote{Priority},
  \dQuote{Depends}, \dQuote{Imports}, \dQuote{LinkingTo},
  \dQuote{Suggests}, \dQuote{Enhances}, \dQuote{OS_type},
  \dQuote{License}, \dQuote{File} and \dQuote{Repository}.
  Additional columns can be specified using the
  \sQuote{fields} argument.
}
\description{
  This is a wrapper around available.packages, with default
  options set.
}
\examples{
options(repos="http://mirror.aarnet.edu.au/pub/CRAN")
p <- available.packages.cran()
str(p)
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link[utils]{available.packages}}
}

