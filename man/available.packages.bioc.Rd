\name{available.packages.bioc}
\alias{available.packages.bioc}
\title{Download a current list of available Bioconductor packages}
\usage{
  available.packages.bioc(prefix = getOption("BioC_mirror"))
}
\arguments{
  \item{prefix}{the URL prefix, eg:
  \url{http://mirror.aarnet.edu.au/pub/CRAN}}
}
\value{
  Same as the output from
  \code{\link[utils]{available.packages}}, but only listing
  packages from Bioconductor: A \code{matrix} with one row
  per package, row names the package names and column names
  \dQuote{Package}, \dQuote{Version}, \dQuote{Priority},
  \dQuote{Depends}, \dQuote{Imports}, \dQuote{LinkingTo},
  \dQuote{Suggests}, \dQuote{Enhances}, \dQuote{OS_type},
  \dQuote{License}, \dQuote{File} and \dQuote{Repository}.
  Additional columns can be specified using the
  \sQuote{fields} argument.
}
\description{
  This is a wrapper around \dQuote{available.packages},
  with default options set.
}
\examples{
# options(BioC_mirror="http://mirror.aarnet.edu.au/pub/bioconductor")
tmp <- available.packages.bioc()
str(tmp)
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link[utils]{available.packages}}
}

