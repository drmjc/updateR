\name{prompt.function}
\alias{prompt.function}
\title{Produce a prototype of an Rd (R Documentation) file, from a function definition.}
\usage{
  prompt.function(object, filename = NULL, name = NULL,
    force.function = FALSE, ...)
}
\arguments{
  \item{object}{an R object, typically a function for the
  default method.  Can be \code{missing} when \code{name}
  is specified.}

  \item{filename}{usually, a connection or a character
  string giving the name of the file to which the
  documentation shell should be written.  The default
  corresponds to a file whose name is \sQuote{name}
  followed by \dQuote{.Rd}.  Can also be \sQuote{NA} (see
  below).}

  \item{name}{a character string specifying the name of the
  object.}

  \item{force.function}{logical: If \code{TRUE}, treat
  \sQuote{object} as function in any case.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  If \sQuote{filename} is \code{NA}, a list-style
  representation of the documentation shell.  Otherwise,
  the name of the file written to is returned invisibly.
}
\description{
  Produce a prototype of an Rd (R Documentation) file, from
  a function definition. In fact, there was never a
  \code{prompt.function} in \code{\link[utils]{prompt}},
  just a \code{\link{prompt.default}}, so I haven't
  over-written anything from \code{utils}. This code
  originally came from
  \dQuote{src/library/utils/R/prompt.R}
}
\author{
  Mark Cowley, 2009-10-13
}

