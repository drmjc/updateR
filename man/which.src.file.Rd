\name{which.src.file}
\alias{which.src.file}
\title{Find the source file for a given function.}
\usage{which.src.file(func, src.root=getOption("src.root"), src.files, exclude.patterns=c("~", ".Rcheck", ".git", ".svn"))
}
\description{Find the source file for a given function.
This is a wrapper around \code{\link{find.src.file}}}
\seealso{\code{\link{find.src.file}}}
\value{a vector of file paths,}
\author{Mark Cowley}
\arguments{\item{func}{a function name, or function's code}
\item{src.root}{The parent folder of source code}
\item{src.files}{an optional vector of filenames to search within}
\item{exclude.patterns}{vector of patterns passed to grep for files to exclude}
\item{unique}{logical: If TRUE then at most 1 file path is returned; if FALSE, all matching
filepaths are returned}
}

