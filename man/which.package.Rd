\name{which.package}
\alias{which.package}
\title{Function to tell you which package a function came from.}
\usage{which.package(func, unique=FALSE, src.root=getOption("src.root"), verbose=FALSE)
}
\description{Function to tell you which package a function came from.}
\details{Given a function, identify which package(s) it was defined in. Most packages
now have namespaces, so functions can either be exported, or not.
If your function is exported in a namespace, then no problems.
If your function is not exported (usually hidden functions starting with \sQuote{.},
eg: \code{.gsea.get.classes.index.html} from \code{metaGSEA},
then there's no facility in native \R to determine this (AFAICT).
By searching through a collection of source code, you may find the file
that contains the function, and thus which package its from.}
\note{If your function is \emph{hidden} (ie, starts with a dot), then you must use 
its quoted name when passing to \code{func=".my.hidden.function"},
otherwise doing this: \code{func=.my.hidden.function}, will cause the \R
interpreter to complain that it can't find \code{.my.hidden.function}.}
\value{a vector of package names, 1 per function in \code{func}}
\author{Mark Cowley}
\arguments{\item{func}{either the quoted function name (eg \dQuote{plot}), or the function's
name (eg \code{plot}). Can be a character vector of multiple function's names.}
\item{unique}{logical: if \code{TRUE}, only return the first result (some functions are
defined in multiple packages), if \code{FALSE} and > 1 instances are found, then all
will be returned}
\item{src.root}{the root folder containing all your R packages under
development.}
\item{verbose}{logical}
}
\examples{\dontrun{
which.package(plot)
}}

