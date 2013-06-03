\name{relibrary}
\alias{relibrary}
\title{Reloads a package}
\usage{
  relibrary(package, character.only = FALSE,
    warn.conflicts = TRUE, ...)
}
\arguments{
  \item{package}{Name or character string giving the name
  of a package.}

  \item{character.only}{A logical indicating whether
  \code{package} can be assumed to be character strings.
  Default value is \code{FALSE}.}

  \item{warn.conflicts}{If \code{TRUE}, warnings are
  printed about conflicts from reattaching of the package,
  unless that package contains an object
  \code{.conflicts.OK}. Default value is \code{FALSE}.}

  \item{\dots}{Any other arguments that
  \code{\link[base]{library}} accepts.}
}
\description{
  Reloads a package. This function works exactly the same
  as \code{\link[base]{library}}, \emph{except} that it is
  reloads the package if it already loaded. This is useful
  for developers. For more information see
  \code{\link[base]{library}}. Hadley Wickham has since
  written the \code{devtools} package which has a very
  similar \code{reload} function.
}
\details{
  While \code{relibrary} is in the process of reloading a
  package the \code{options(relibrary)} will be set to name
  of the package currently being reloaded. This can be
  useful if the package to be reloaded would like save away
  data until it is loaded again.

  As of R 2.12, (and perhaps before) reloading of
  documentation files doesn't appear to be working. If you
  change doc files, then for the time being, the whole R
  session needs restarting.Relibrary does still work OK for
  updating source code changes.
}
\note{
  Reloading packages after making changes to
  documentation\cr If you edit an Rd file, rebuild,
  reinstall & reload the package in an active R session,
  you will get this error:\cr \dQuote{\code{Error in
  fetch(key) : internal error -3 in R_decompress1}} I've
  confirmed with Rdevel (2012-01-27) that it is impossible
  to work around this error\dots you have to restart your R
  session.\cr Prof Ripley: \dQuote{This is simply not
  supported.  Lazy-load databases are cached, and you
  cannot expect to change them during the R session once
  they have been used.} More info, including 3 suggestions
  here:
  \url{http://r.789695.n4.nabble.com/Unable-to-reload-Rdoc-td4333063.html}
  Hadley's \code{devtools::check_Rd} function will render
  an Rd file without having to relaod it.
}
\author{
  Henrik Bengtsson, \email{henrikb@braju.com},
  \url{http://www.braju.com/R/}, updated by Mark Cowley.
}
\references{
  \url{https://stat.ethz.ch/pipermail/r-help/2002-January/018006.html}
  \url{http://r.789695.n4.nabble.com/Unable-to-reload-Rdoc-td4333063.html}
}
\seealso{
  See \code{\link[base]{library}} \code{\link{unlibrary}}
}

