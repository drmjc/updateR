\name{.get.dependencies}
\alias{.get.dependencies}
\title{Private function to recursively determine the dependencies of a given function.}
\usage{.get.dependencies(fun, packages, ignore.packages=c("base", getOption("defaultPackages")),
    src.root=getOption("src.root"), verbose=FALSE)
.get.dependencies(fun, packages, ignore.packages=c("base", getOption("defaultPackages")),
    src.root=getOption("src.root"), verbose=FALSE)
.get.dependencies(fun, packages, ignore.packages=c("base", getOption("defaultPackages")),
    src.root=getOption("src.root"), verbose=FALSE)
.get.dependencies(fun, packages, ignore.packages=c("base", getOption("defaultPackages")),
    src.root=getOption("src.root"), verbose=FALSE)
.get.dependencies(fun, packages, ignore.packages=c("base", getOption("defaultPackages")),
    src.root=getOption("src.root"), verbose=FALSE)
.get.dependencies(fun, packages, ignore.packages=c("base", getOption("defaultPackages")),
    src.root=getOption("src.root"), verbose=FALSE)
}
\description{Private function to recursively determine the dependencies of a given function.}
\details{Not intended for public consumption. See get.dependencies\code{.get.dependencies}: Not intended for public consumption. See get.dependencies


\code{.get.dependencies}: Not intended for public consumption. See get.dependencies


\code{.get.dependencies}: Not intended for public consumption. See get.dependencies


\code{.get.dependencies}: Not intended for public consumption. See get.dependencies


\code{.get.dependencies}: Not intended for public consumption. See get.dependencies


}
\value{a list with these elements:
\item{my.functions}{a vector of all functions that are called}
\item{my.function.packages}{a vector indicating which package each function came from}
\item{other.packages}{a vector of other CRAN or BioConductor packages which are also needed}\code{.get.dependencies}: a list with these elements:
\item{my.functions}{a vector of all functions that are called}
\item{my.function.packages}{a vector indicating which package each function came from}
\item{other.packages}{a vector of other CRAN or BioConductor packages which are also needed}


\code{.get.dependencies}: a list with these elements:
\item{my.functions}{a vector of all functions that are called}
\item{my.function.packages}{a vector indicating which package each function came from}
\item{other.packages}{a vector of other CRAN or BioConductor packages which are also needed}


\code{.get.dependencies}: a list with these elements:
\item{my.functions}{a vector of all functions that are called}
\item{my.function.packages}{a vector indicating which package each function came from}
\item{other.packages}{a vector of other CRAN or BioConductor packages which are also needed}


\code{.get.dependencies}: a list with these elements:
\item{my.functions}{a vector of all functions that are called}
\item{my.function.packages}{a vector indicating which package each function came from}
\item{other.packages}{a vector of other CRAN or BioConductor packages which are also needed}


\code{.get.dependencies}: a list with these elements:
\item{my.functions}{a vector of all functions that are called}
\item{my.function.packages}{a vector indicating which package each function came from}
\item{other.packages}{a vector of other CRAN or BioConductor packages which are also needed}


}
\author{Mark Cowley}
\arguments{\item{fun}{either a function name, of the function's code}
\item{packages}{vector of packages to search for dependencies in (ie packages that you probably wrote)}
\item{ignore.packages}{a vector of packages to ignore. defaults to the list of pacakges loaded at startup.}
\item{verbose}{print the recursive dependency trace}
}

\alias{.get.dependencies}

\alias{.get.dependencies}

\alias{.get.dependencies}

\alias{.get.dependencies}

\alias{.get.dependencies}

