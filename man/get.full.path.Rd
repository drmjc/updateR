\name{get.full.path}
\alias{get.full.path}
\title{Expand a file path to an absolute path...}
\usage{get.full.path(path, trailing.slash=TRUE)
}
\description{Expand a file path to an absolute path}
\details{This function has more capabilities than path.expand, in that relative
paths (. and ./ and ../) are also expanded.}
\author{Mark Cowley}
\arguments{\item{path}{A character vector of paths to files or directories}
\item{trailing.slash}{logical. if TRUE, and path is a directory, include a trailing \dQuote{/}}
}
\examples{get.full.path("~", trailing.slash=FALSE)
get.full.path("~", trailing.slash=TRUE)
get.full.path("./")
get.full.path("..")}

