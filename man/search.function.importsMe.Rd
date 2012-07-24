\name{search.function.importsMe}
\alias{search.function.importsMe}
\title{find the functions, which @importFrom 'func'}
\usage{
  search.function.importsMe(func, pkg,
    src.root = getOption("src.root", "~/src/R"))
}
\arguments{
  \item{pkg}{the name of a package which contains
  \code{func}}

  \item{func}{function or function name}

  \item{src.root}{the root directory containing your
  codebase.}
}
\value{
  a \code{list} of elements, 1 per file in which the
  @importFrom directive is found. each element contains:\cr
  \item{package}{the package name containing the import
  directive} \item{srcref}{The path to the file containing
  the import directive} \item{funcs}{a charcter vector of
  function name(s) which contain the import directive} If
  the element is not found, an empty \code{list} is
  returned.
}
\description{
  search within a code base defined by \code{src.root}, for
  functions which contain @importFrom 'pkg' 'func'
}
\examples{
search.function.importsMe("rename.column", "pwbc")
}
\author{
  Mark Cowley, 2012-07-16
}

