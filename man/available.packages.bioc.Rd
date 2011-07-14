\name{available.packages.bioc}
\alias{available.packages.bioc}
\title{Download a current list of available Bioconductor packages...}
\usage{available.packages.bioc(prefix=getOption("BioC_mirror"))
}
\description{Download a current list of available Bioconductor packages}
\details{This is a wrapper around available.packages, with default options set.}
\value{See \code{\link[utils]{available.packages}}}
\seealso{\code{\link[utils]{available.packages}}}
\author{Mark Cowley}
\arguments{\item{prefix}{the URL prefix, eg: \url{http://mirror.aarnet.edu.au/pub/CRAN}}
}
\examples{options(BioC_mirror="http://mirror.aarnet.edu.au/pub/bioconductor")
available.packages.bioc()}

