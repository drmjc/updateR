\name{refactoR}
\alias{refactoR}
\title{refactoR R code}
\usage{
  refactoR(func, source.pkg, dest.pkg,
    src.root = getOption("src.root", "~/src/R"))
}
\arguments{
  \item{func}{function or function name}

  \item{source.pkg}{character(1) of the source package name
  (ie where the function is currently defined)}

  \item{dest.pkg}{character(1) of the destination package
  name (ie where you want the function to be defined)}

  \item{src.root}{the root directory containing your
  codebase.}
}
\value{
  undefined.
}
\description{
  under development.
}
\author{
  Mark Cowley, 2012-07-20
}

