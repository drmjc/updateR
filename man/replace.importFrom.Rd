\name{replace.importFrom}
\alias{replace.importFrom}
\title{replace the @importFrom statements with a new package}
\usage{
  replace.importFrom(func, source.pkg, dest.pkg, file)
}
\arguments{
  \item{func}{function or function name}

  \item{source.pkg}{character(1) of the source package name
  (ie where the function is currently defined)}

  \item{dest.pkg}{character(1) of the destination package
  name (ie where you want the function to be defined)}
}
\value{
  nothing it will edit the src file, so be very careful!
}
\description{
  Given a \code{file} which contains an \code{@importFrom
  source.pkg func} directive, replace this with
  \code{@importFrom dest.pkg func}
}
\author{
  Mark Cowley, 2012-07-16
}

