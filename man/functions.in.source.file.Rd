\name{functions.in.source.file}
\alias{functions.in.source.file}
\title{list the function names defined in a source file}
\usage{
  functions.in.source.file(f)
}
\arguments{
  \item{f}{the path to a .R source file}
}
\value{
  a character vector of function names
}
\description{
  list the function names defined in a source file
}
\section{TODO}{
  this only works with "<- function". It doesn't work with
  "= function", nor with S4 functions.
}
\examples{
# debugonce(functions.in.source.file)
functions.in.source.file("~/src/R/metaGSEA/R/export.gsea.gct.R")
functions.in.source.file("~/src/R/mjcdev/R/igraph.utils.R")
# functions.in.source.file("/misc/ICGCPancreas/PINA/bin/GeneSetCollection-methods.R")
}
\author{
  Mark Cowley, 2011-11-10
}

