\name{get.function.dependencies}
\alias{get.function.dependencies}
\title{Determine all the dependencies of a function from a list of packages.}
\usage{
  get.function.dependencies(fun,
    packages = c(list.my.packages(src.root = getOption("src.root")), ".GlobalEnv"),
    verbose = FALSE)
}
\arguments{
  \item{fun}{either a function name, of the function's
  code}

  \item{packages}{vector of packages to search for
  dependencies in (ie packages that you probably wrote),
  Include \code{.GlobalEnv} if you want to trace inside
  functions which may be loaded in the workspace.}

  \item{verbose}{print the recursive dependency trace}
}
\value{
  A list with these elements: \item{my.functions}{a
  character vector of all functions that are called, that
  are also in the \code{my.packages} packages}
  \item{my.function.packages}{a vector indicating which
  package each function came from} \item{cran}{a vector of
  CRAN packages that are referenced} \item{bioc}{a vector
  of BioConductor packages that are referenced}
  \item{other.packages:}{a vector of other packages not
  from CRAN, or BioConductor, or my.packages.}
}
\description{
  For a given function, exhaustively determine which
  functions are in turn called by that function. This is
  designed to search within a subset of packages - perhaps
  ones that you are actively coding. This method will also
  tell you which CRAN or BioConductor packages are also
  referenced.
}
\details{
  If you've ever wanted to give someone a single function
  from within a package full of code, then you've probably
  forgotten about some of the code that this function
  depends upon, which you wrote & which is not available on
  CRAN/BioC. Given a single function, this method will
  determine all the code dependencies, ie all the functions
  which will ever get called. This is exhaustive, in that
  it doesn't only look for functions inside conditional
  statements -- all functions are returned. It only
  searches within a limited set of packages, such as those
  that you may have written yourself & not published yet.
  It will also tell you which CRAN/BioC packages need to be
  installed, but will ignore those packages which are
  automatically loaded at startup.
}
\examples{
options(src.root="~/src/R")
get.function.dependencies("updateR")
get.function.dependencies(updateR)
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link[codetools]{findGlobals}},
  \code{\link{list.my.packages}}
}
\keyword{utilities}

