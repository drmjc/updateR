\name{updateR}
\alias{updateR}
\title{A flexible R package updater.}
\usage{
  updateR(package, src.root = getOption("src.root"),
    lib.loc = NULL, warn.conflicts = TRUE, roxygen = FALSE,
    check = FALSE, source = TRUE, binary = FALSE,
    winbinary = FALSE, install = TRUE, test = FALSE,
    deploy = FALSE, no.vignettes = FALSE,
    no.manual = FALSE, no.docs = FALSE,
    no.examples = FALSE)
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

  \item{test}{logical: run a
  \code{testthat::\link[testthat]{test_package}} suite, on
  the installed version of the package? Note this is done
  after this function is given the opportunity to install
  the package.}

  \item{deploy}{logical: if \code{TRUE}, then deploy the
  \code{package_VERSION.tar.gz} to\cr enzo. Note if you
  don't build a new package (ie \code{source=FALSE}, and
  \code{binary=FALSE}), then this will deploy the existing
  version of the package bundle, else the newly created
  package bundle will be deployed.}

  \item{no.vignettes}{logical: if \dQuote{TRUE}, turn off
  the creation of vignettes}

  \item{no.manual}{logical: if \dQuote{TRUE}, turn off the
  creation of PDF manuals}

  \item{no.docs}{logical: if \dQuote{TRUE}, turn off the
  creation of documentation}
}
\description{
  This function can be used to roxygenize, check, build
  source/binary/windows binary, install, reload, test and
  deploy R packages. Under-the-hood, it calls a shell
  script, which in turn calls various \dQuote{R CMD}
  programs, like R CMD {CHECK,BUILDINSTALL}, in addition to
  some others, including roxygen, testthat. This function
  assumes that you develop your R packages within a top
  level directory, eg ~/src/R given by the
  \code{options("src.root")} option.
}
\details{
  I tend to edit my source code in an external editor
  (TextMate), then will want to incorporate those code
  changes into my current R session. A simple
  updateR("my.package", source=TRUE, install=TRUE), causes
  the package to be built and then installed. Optional
  roxygenizing & package path checking can be run prior to
  package building;\cr source or binary package bundle
  building;\cr installation, reloading the package in the
  current session;\cr followed by testing via Hadley
  Wickham's \code{testthat} and deployment to enzo after
  installation.
}
\note{
  Currently, \code{\link{relibrary}} is unable to update
  the R documentation objects, giving internal errors.
}
\section{Roxygen2}{
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

\section{Checking}{
  \code{R CMD CHECK} is run on the package folder, not the
  package bundle. It's on my todo list to supporte
  \code{CHECK}ing of the tar.gz, possibly with the
  \code{--as-cran} option.
}

\section{Testing}{
  If the package contains a testthat package suite, then
  selecting \code{test=TRUE} will run a
  \code{\link[testthat]{test_package}} on the
  \code{package}.
}

\section{Deploy}{
  if \code{deploy=TRUE}, then the newly built package
  bundle will be deployed, ie copied and installed to enzo.
  Note that if you run updateR with \code{source=FALSE,
  binary=FALSE}, then the existing package bundle will be
  deployed. This assumes that you have setup password-less
  authentication on the target host, and the username is
  the same as on the submission machine.
}

\section{in case of epic fail}{
  \verb{ bash$ ~/src/R/updateR/inst/bin/updateR.sh
  ~/src/R/updateR R> relibrary("updateR") }
}

\section{TODO}{
  \describe{ \item{devtools}{Investigate more of the
  \code{devtools} functions for build/check/install.}
  \item{Checking bundle}{Run R CMD CHECK --as-cran on the
  package bundle. Why? 'cos that's what\cr CRAN runs their
  checks on -- not the folder which may mask errors due to
  the way i've setup my environment.} }
}
\examples{
\dontrun{
# build a source package, and install.
updateR("updateR", "~/src/R")
updateR("updateR", "~/src/R", source=TRUE, install=TRUE)
# roxygenize, check, and then build a source package, and install.
updateR("updateR", "~/src/R", roxygen=TRUE, check=TRUE, source=TRUE, install=TRUE)
# roxygenize, check, and then build a source package, and install + test & deploy.
updateR("updateR", "~/src/R", roxygen=TRUE, check=TRUE, source=TRUE, install=TRUE, test=TRUE, deploy=TRUE)
}
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link{relibrary}},
  \code{\link[roxygen2]{roxygenize}},
  \code{\link{.Rprofile}}, \code{\link{install.packages}}
}

