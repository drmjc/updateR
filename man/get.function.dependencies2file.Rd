\name{get.function.dependencies2file}
\alias{get.function.dependencies2file}
\title{Determine all the dependencies of a function from a list of packages and src files and export them into a file...}
\usage{get.function.dependencies2file(fun, file, packages=c(list.my.packages(src.root =
    getOption("src.root")), ".GlobalEnv"), src.files, verbose=FALSE)
}
\description{Determine all the dependencies of a function from a list of packages and src files and export them into a file}
\details{If you've ever wanted to give someone a single function from within a package full of code, then you've probably
forgotten a few other code functions & gone back & forth.
Given a single function, this method will determine all the code dependencies. 
It only searches within a limited set of packages, such as those that you may have written yourself & not published
yet.
It will also tell you which CRAN/BioC packages need to be installed, but will ignore those packages which are
automatically loaded at startup.}
\value{nothing. writes a file containing source code.}
\author{Mark Cowley, 2011-04-07}
\arguments{\item{fun}{either a function name, of the function's code}
\item{file}{path to a file to write to}
\item{packages}{vector of packages to search for dependencies in (ie packages that you probably wrote)}
\item{src.files}{optional vector of R sources files to also search within. These have higher priority than code
within packages.}
\item{verbose}{logical: verbose output?}
}
\examples{\dontrun{
f <- tempfile()
get.function.dependencies2file(fun=edit.src.file, file=f)
get.function.dependencies2file(fun=edit.src.file, file=f, src.files=NULL, verbose=TRUE)
unlink(f)
}}

