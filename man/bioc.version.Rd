\name{bioc.version}
\alias{bioc.version}
\title{What version of Bioconductor does this version of R use?}
\usage{
  bioc.version()
}
\value{
  The current version of bioc as a character string, eg
  "2.7"
}
\description{
  Since at least R 2.5, Bioc has been 5 minor points
  behind, so R 2.5 = bioc 2.0 R 2.12 = bioc 2.7 ...
}
\examples{
bioc.version()
}
\author{
  Mark Cowley
}

