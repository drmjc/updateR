\name{edit.src.file}
\alias{edit.src.file}
\title{Search for the...}
\usage{edit.src.file(func, src.root=getOption("src.root"), src.files, editor,
    exclude.patterns=c("~", ".Rcheck", ".git", ".svn"))
}
\description{Search for the .R file that contains a function, and open it in an editor.}
\value{nothing. The side effect is that an R source file is opened in an editor.}
\author{Mark Cowley, 26/2/07}
\arguments{\item{func}{a function name, or the function's code}
\item{src.root}{The parent folder of source code}
\item{src.files}{an optional vector of filenames to search within}
\item{editor}{the path to your editor.}
\item{exclude.patterns}{vector of patterns passed to grep for files to exclude}
}

