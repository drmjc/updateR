\name{find.src.file}
\alias{find.src.file}
\title{Find the source file for a given function.}
\usage{
  find.src.file(func, src.root = getOption("src.root"),
    src.files = NULL,
    exclude.patterns = c("~", ".Rcheck", ".git", ".svn"),
    unique = TRUE, exclude.symlinks = TRUE)
}
\arguments{
  \item{func}{a function name, or function's code}

  \item{src.root}{The parent folder of source code}

  \item{src.files}{an optional vector of filenames to
  search within}

  \item{exclude.patterns}{vector of patterns passed to grep
  for files to exclude}

  \item{unique}{logical: If \code{TRUE} then at most 1 file
  path is returned; if \code{FALSE}, all matching filepaths
  are returned}

  \item{exclude.symlinks}{logical: if \code{TRUE}, then
  only report files which are not symbolic links.}
}
\value{
  a vector of file paths
}
\description{
  Given a directory of R packages under development (under
  src.root), or a vector of R files, given by src.files,
  determine the name of the source file which func was
  defined within.
}
\author{
  Mark Cowley
}

