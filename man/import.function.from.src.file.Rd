\name{import.function.from.src.file}
\alias{import.function.from.src.file}
\title{Import the function definition from within a source file.}
\usage{
  import.function.from.src.file(func, f,
    import.header = TRUE)
}
\arguments{
  \item{func}{a function name, or the function's body. eg
  \code{plot} or \dQuote{\code{plot}}}

  \item{f}{The path to the file containing the function.}

  \item{import.header}{search for & import the header
  comments. TRUE/FALSE}
}
\value{
  This code extracts the src code, and the header, or
  returns ""
}
\description{
  I usually write detailed comment headers before functions
  in source code files. scenarios: 1. function header
  preceded by comment block preceded by a blank line, or
  top of file 2. function header preceded by (blank line or
  top of file) -> no comment has been written.
}
\author{
  Mark Cowley, 2011-06-21
}

