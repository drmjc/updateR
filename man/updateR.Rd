\name{updateR}
\alias{updateR}
\title{A flexible R package updater.}
\usage{
  updateR(package, src.root = getOption("src.root"),
    lib.loc = NULL, warn.conflicts = TRUE, roxygen = FALSE,
    check.source = FALSE, source = TRUE, binary = FALSE,
    winbinary = FALSE, no.vignettes = FALSE,
    no.manual = FALSE, no.docs = FALSE,
    check.bundle = FALSE, install = TRUE, test = FALSE,
    deploy = NULL, no.examples = FALSE)
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

  \item{check.source}{logical: \code{R CMD CHECK} the
  package prior to building it?}

  \item{source}{logical: Build a source package? If
  \dQuote{TRUE}, and \dQuote{install=TRUE}, then this is
  the package that will be installed.}

  \item{binary}{logical: Build a binary package?}

  \item{winbinary}{logical: Build a windows binary
  package?}

  \item{install}{logical: \code{R CMD INSTALL} the
  package?}

  \item{test}{logical: run a
  \code{testthat::\link[testthat]{test_package}} suite, on
  the installed version of the package? Note this is done
  after this function is given the opportunity to install
  the package.}

  \item{deploy}{either \code{NULL}, or the hostname to SCP
  and R CMD INSTALL the package bundle to.  see details.}

  \item{no.vignettes}{logical: if \dQuote{TRUE}, turn off
  the creation of vignettes}

  \item{no.manual}{logical: if \dQuote{TRUE}, turn off the
  creation of PDF manuals}

  \item{no.docs}{logical: if \dQuote{TRUE}, turn off the
  creation of documentation}
}
\description{
  This function can be used to roxygenize, check source,
  build (source/binary/windows binary), check bundle,
  install, reload in active R session (see notes), test and
  deploy R packages. Under-the-hood, it calls a shell
  script, which in turn calls various \dQuote{R CMD}
  programs, like R CMD {CHECK,BUILD,INSTALL}, as well as
  roxygen2 & testthat. This function assumes that you
  develop your R packages within a top level directory, eg
  \code{~/src/R} given by the \code{options("src.root")}
  option.
}
\details{
  \code{Example workflow}: These are the steps during a
  typical package development cycle, which are supported by
  updateR:\cr roxygenize mypackage (\code{roxygenize})\cr R
  CMD CHECK mypackage (\code{check.source})\cr R CMD BUILD
  --binary mypackage (binary)\cr R CMD BUILD mypackage
  (source)\cr R CMD CHECK mypackage.tar.gz
  (\code{check.bundle})\cr R CMD INSTALL mypackage.tar.gz
  (\code{install})\cr testthat::test_package("mypackage")
  (\code{test})\cr reload package in current \R session
  (\code{reload})\cr deploy to remote host (SCP and R CMD
  INSTALL on host: \code{deploy})\cr Typically, a simple
  \code{updateR("my.package", source=TRUE, install=TRUE)},
  causes the package to be built, installed & reloaded. \cr
  or from the command line, \code{updateR.sh -r -s -i
  ./mypackage}.
}
\section{roxygenize}{
  roxygen2 is an inline documentation engine which builds
  \code{Rd} files from structured comment headers above
  each function. roxygen2 creates \code{Rd} files, and
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

\section{CHECKing}{
  \code{R CMD CHECK} is run either on the source code
  (\code{check.source}), or the built package bundle
  (\code{check.source}). In the latter case, \code{R CMD
  CHECK --as-cran} is used.
}

\section{INSTALLing}{
  R CMD INSTALL will be run as the current user, thus the
  package will be installed to the typical location where
  you have permission. On Linux this is usually a
  user-specific \R-library, but on OSX, this is usually the
  global \R-library.
}

\section{BUILDing}{
  The package source can be BUILT into a \code{source}, or
  \code{binary} bundle, or both. Currently building a
  windows binary (\code{winbinary}) package is unsupported.
  If you need a windows binary from a non-windows machine,
  then check out \url{http://win-builder.r-project.org/}.
  Note that since \R-2.14, building binary packages is
  always preceded by installing the package. See REFERENCES
  in updateR.sh for more info.
}

\section{Testing}{
  If the package contains a \code{testthat} package suite,
  then selecting \code{test=TRUE} will run a
  \code{\link[testthat]{test_package}} on the
  \code{package}.
}

\section{Deploy}{
  if \code{deploy="hostname"}, then the newly built package
  bundle will be deployed, ie SCP'ed and installed to that
  host. Note that if you run updateR with
  \code{source=FALSE, binary=FALSE}, then the existing
  package bundle will be deployed.\cr This handles a few
  scenarios:\cr [1] local and remote usernames are
  identical, and you have setup password-less
  authentication, then just set
  \code{deploy="hostname"}.\cr [2] local and remote
  usernames differ, and you have setup password-less
  authentication, then set
  \code{deploy="username@hostname"}\cr [3] you have to
  enter an SCP and SSH password, then\cr set
  \code{deploy="username@hostname -p mypassword"}\cr All of
  these scenario's will install the package to whereever
  you have rights to do so.
}

\section{relibrary}{
  Once code has been updated, and INSTALLed, you can reload
  the package in the current \R session. Note this can't be
  done from the commandline. This generally works well, but
  since the R documentation files are cached in a lazyload
  db, it's impossible to update the documents in the same
  \R-session, and you will get this error:\cr
  \dQuote{\code{Error in fetch(key) : internal error -3 in
  R_decompress1}}\cr see \code{\link{reload}} for more
  info.
}

\section{TODO}{
  \describe{ \item{devtools}{Investigate more of Hadley's
  \code{devtools} functions for build/check/install.} }
}
\examples{
\dontrun{
# build a source package, and install.
updateR("updateR", "~/src/R")
updateR("updateR", "~/src/R", source=TRUE, install=TRUE)
# roxygenize, check.source, and then build a source package, and install.
updateR("updateR", "~/src/R", roxygen=TRUE, check.source=TRUE, source=TRUE, install=TRUE)
# roxygenize, check.source, and then build a source package, and install + test & deploy.
updateR("updateR", "~/src/R", roxygen=TRUE, check.source=TRUE, source=TRUE, install=TRUE, test=TRUE, deploy="enzo")
}
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link{relibrary}},
  \code{\link[roxygen2]{roxygenize}},
  \code{\link[testthat]{test_package}},
  \code{\link{.Rprofile}}, \code{\link{install.packages}}
}

