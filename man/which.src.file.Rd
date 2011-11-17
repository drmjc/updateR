\name{which.src.file}
\alias{which.src.file}
\title{Find the source file for a given function.
This is a wrapper around \code{\link{find.src.file}}}
\usage{
  which.src.file(func, src.root = getOption("src.root"),
  src.files = NULL, exclude.patterns = c("~", ".Rcheck",
  ".git", ".svn"), unique = TRUE)
}
\arguments{
  \item{func}{a function name, or function's code}

  \item{src.root}{The parent folder of source code}

  \item{src.files}{an optional vector of filenames to
  search within}

  \item{exclude.patterns}{vector of patterns passed to grep
  for files to exclude}

  \item{unique}{logical: If TRUE then at most 1 file path
  is returned; if FALSE, all matching filepaths are
  returned}
}
\value{
  a vector of file paths,
}
\description{
  Find the source file for a given function. This is a
  wrapper around \code{\link{find.src.file}}
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link{find.src.file}}
}

