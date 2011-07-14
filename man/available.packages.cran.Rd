\name{available.packages.cran}
\alias{available.packages.cran}
\title{Download a current list of available CRAN packages...}
\usage{available.packages.cran(prefix=getOption("repos"))
}
\description{Download a current list of available CRAN packages}
\details{This is a wrapper around available.packages, with default options set.}
\value{See \code{\link[utils]{available.packages}}}
\seealso{\code{\link[utils]{available.packages}}}
\author{Mark Cowley}
\arguments{\item{prefix}{the URL prefix, eg: \url{http://mirror.aarnet.edu.au/pub/CRAN}}
}
\examples{options(repos="http://mirror.aarnet.edu.au/pub/CRAN")
available.packages.cran()}

