\name{mate}
\alias{mate}
\title{Edit source file in TextMate}
\usage{
  mate(func, src.root = getOption("src.root"))
}
\arguments{
  \item{func}{a function name, or the function's code}

  \item{src.root}{The parent folder of source code}
}
\value{
  nothing. The side effect is that an R source file is
  opened in an editor.
}
\description{
  Edit source file in TextMate
}
\author{
  Mark Cowley, 26/2/07
}

