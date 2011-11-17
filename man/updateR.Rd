\name{updateR}
\alias{updateR}
\title{A flexible R package updater.
This function can be used to roxygenize, check, build source/binary/windows binary, install
and reload R packages.
Under-the-hood, it calls a shell script, which in turn calls various \dQuote{R CMD}
programs, like R CMD CHECK, R CMD roxygenize, R CMD BUILD, R CMD INSTALL.
This function assumes that you develop your R packages within a top level directory, eg ~/src/R
given by the \dQuote{src.root} option.}
\usage{
  updateR(package, src.root = getOption("src.root"),
  lib.loc = NULL, warn.conflicts = TRUE, roxygen = FALSE,
  check = FALSE, source = TRUE, binary = FALSE, winbinary =
  FALSE, install = TRUE, no.vignettes = FALSE, no.manual =
  FALSE, no.docs = FALSE)
}
\arguments{
  \item{package}{the name of the package to be updated.
  either quoted, or unquoted.}

  \item{src.root}{the path to the root of the src files.}

  \item{lib.loc}{where to install the library. Defaults to
  .libPaths()[1]}

  \item{warn.conflicts}{see relibrary}

  \item{roxygen}{logical: roxygenize the package (generate
  Rd files on the fly)?}

  \item{check}{logical: \code{R CMD CHECK} the package
  prior to building it?}

  \item{source}{logical: Build a source package? If
  \dQuote{TRUE}, and \dQuote{install=TRUE}, then this is
  the package that will be installed.}

  \item{binary}{logical: Build a binary package?}

  \item{winbinary}{logical: Build a windows binary
  package?}

  \item{install}{logical: \code{R CMD INSTALL} the
  package?}

  \item{no.vignettes}{logical: if \dQuote{TRUE}, turn off
  the creation of vignettes}

  \item{no.manual}{logical: if \dQuote{TRUE}, turn off the
  creation of PDF manuals}

  \item{no.docs}{if TRUE, logical: if \dQuote{TRUE}, turn
  off the creation of documentation}
}
\description{
  I tend to edit my source code in an external editor
  (TextMate), then will want to incorporate those code
  changes into my current R session. A simple
  updateR("my.package", source=TRUE, install=TRUE), causes
  the package to be built and then installed. Optional
  roxygenizing, and package checking can occur, prior to
  package building.
}
\details{
  Roxygen is an inline documentation engine which builds
  \code{Rd} files from structured comment headers above
  each function. Roxygen creates \code{Rd} files, and
  updates the \code{NAMESPACE} and the Date and Collates
  fields in the \code{DESCRIPTION} files, however it
  normally does this in a separate copy of the package.
  \code{updateR} removes all previously existing \code{Rd}
  files & replaces them with new \code{Rd} files, and
  merges the changes made to \code{NAMESPACE} and
  \code{DESCRIPTION} into the package directory. WARNING
  this will delete any files which are only encoded in the
  \code{Rd} format, and not in roxygen comments. Try the
  \code{\link[Rd2roxygen]{Rd2roxygen}} package for
  converting from \code{Rd} to \code{roxygen} comments.
}
\note{
  Currently, \code{\link{relibrary}} is unable to update
  the R documentation objects, giving internal errors.

  in case of epic fail: \verb{ bash$
  ~/src/R/updateR/inst/bin/updateR.sh ~/src/R/updateR R>
  relibrary("updateR") }
}
\examples{
\dontrun{
# build a source package, and install.
updateR("updateR", "~/src/R")
updateR("updateR", "~/src/R", source=TRUE, install=TRUE)
# roxygenize, check, and then build a source package, and install.
updateR("updateR", "~/src/R", roxygen=TRUE, check=TRUE, source=TRUE, install=TRUE)
}
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link{relibrary}},
  \code{\link[roxygen]{roxygenize}},
  \code{\link{.Rprofile}}, \code{\link{install.packages}}
}

