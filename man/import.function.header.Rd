\name{import.function.header}
\alias{import.function.header}
\title{Search for & import the function header from within a source file.}
\usage{
  import.function.header(func,
    src.root = getOption("src.root"), src.files = NULL,
    strip.comment.char = FALSE,
    exclude.patterns = c("~", ".Rcheck", ".git", ".svn"))
}
\arguments{
  \item{func}{a function name, or the function's code}

  \item{src.root}{The parent folder of source code}

  \item{src.files}{an optional vector of filenames to
  search within}

  \item{strip.comment.char}{logical: if TRUE, strip the
  leading comment characters.}

  \item{exclude.patterns}{vector of patterns passed to grep
  for files to exclude}
}
\value{
  This code extracts the src code, and the header, or
  returns ""
}
\description{
  I usually write detailed comment headers before functions
  in source code files. scenarios: 1. function header
  preceded by comment block preceded by (blank line, or top
  of file) 2. function header preceded by (blank line or
  top of file) -> no comment has been written.
}
\author{
  Mark Cowley, 2009-10-13
}

