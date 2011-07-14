\name{find.src.file}
\alias{find.src.file}
\title{Find the source file for a given function.}
\usage{find.src.file(func, src.root=getOption("src.root"), src.files, exclude.patterns=c("~", ".Rcheck", ".git", ".svn"), unique=TRUE)
}
\description{Find the source file for a given function.}
\details{Given a directory of R packages under development (under src.root), or a vector of
R files, given by src.files, determine the name of the source file which func
was defined within.}
\value{a vector of file paths,}
\author{Mark Cowley}
\arguments{\item{func}{a function name, or function's code}
\item{src.root}{The parent folder of source code}
\item{src.files}{an optional vector of filenames to search within}
\item{exclude.patterns}{vector of patterns passed to grep for files to exclude}
\item{unique}{logical: If TRUE then at most 1 file path is returned; if FALSE, all matching
filepaths are returned}
}

