\name{import.function.header.from.src.file}
\alias{import.function.header.from.src.file}
\title{Import the function header from within a source file.}
\usage{import.function.header.from.src.file(func, f, strip.comment.char=FALSE)
}
\description{Import the function header from within a source file.}
\details{I usually write detailed comment headers before functions in source code files.
scenarios:
1. function header preceded by comment block preceded by (blank line, or top of file)
2. function header preceded by (blank line or top of file) -> no comment has been written.}
\value{This code extracts the src code, and the header, or returns ""}
\author{Mark Cowley, 2009-10-13}
\arguments{\item{func}{a function name, or the function's code}
\item{f}{The path to the file containing the function.}
\item{exclude.patterns}{vector of patterns passed to grep for files to exclude}
}
