#' A flexible R package updater.
#' 
#' This function can be used to roxygenize, check source, build (source/binary/windows binary), check bundle,
#' install, reload in active R session (see notes), test and deploy R packages. 
#' Under-the-hood, it calls a shell script, which in turn calls various \dQuote{R CMD}
#' programs, like R CMD {CHECK,BUILD,INSTALL}, as well as roxygen2 & testthat.
#' This function assumes that you develop your R packages within a top level directory, eg \code{~/src/R}
#' given by the \code{options("src.root")} option.
#' 
#' @details \code{Example workflow}: 
#' These are the steps during a typical package development cycle, which are supported by updateR:\cr
#' roxygenize mypackage (\code{roxygenize})\cr
#' R CMD CHECK mypackage (\code{check.source})\cr
#' R CMD BUILD --binary mypackage (binary)\cr
#' R CMD BUILD mypackage (source)\cr
#' R CMD CHECK mypackage.tar.gz (\code{check.bundle})\cr
#' R CMD INSTALL mypackage.tar.gz (\code{install})\cr
#' testthat::test_package("mypackage") (\code{test})\cr
#' reload package in current \R session (\code{reload})\cr
#' deploy to remote host (SCP and R CMD INSTALL on host: \code{deploy})\cr
#' Typically, a simple \code{updateR("my.package", source=TRUE, install=TRUE)},
#' causes the package to be built, installed & reloaded. \cr
#' or from the command line, \code{updateR.sh -r -s -i ./mypackage}.
#' 
#' @section roxygenize:
#' roxygen2 is an inline documentation engine which builds \code{Rd} files from structured comment 
#' headers above each function. roxygen2 creates \code{Rd} files, and updates the \code{NAMESPACE} and
#' the Date and Collates fields in the \code{DESCRIPTION} files, however it normally does this in a 
#' separate copy of the package.
#' \code{updateR} removes all previously existing \code{Rd} files & replaces them with new \code{Rd} 
#' files, and merges the changes made to \code{NAMESPACE} and \code{DESCRIPTION} into the package 
#' directory.
#' WARNING this will delete any files which are only encoded in the \code{Rd} format, and not in
#' roxygen comments. Try the \code{\link[Rd2roxygen]{Rd2roxygen}} package for converting from 
#' \code{Rd} to \code{roxygen} comments.
#'
#' @section CHECKing:
#' \code{R CMD CHECK} is run either on the source code (\code{check.source}), or the built package bundle (\code{check.source}).
#' In the latter case, \code{R CMD CHECK --as-cran} is used.
#' 
#' @section INSTALLing:
#' R CMD INSTALL will be run as the current user, thus the package will be installed to the typical
#' location where you have permission. On Linux this is usually a user-specific \R-library, but on
#' OSX, this is usually the global \R-library.
#' 
#' @section BUILDing:
#' The package source can be BUILT into a \code{source}, or \code{binary} bundle, or both. Currently building a
#' windows binary (\code{winbinary}) package is unsupported. If you need a windows binary from a non-windows machine,
#' then check out \url{http://win-builder.r-project.org/}. Note that since \R-2.14, building binary
#' packages is always preceded by installing the package. See REFERENCES in updateR.sh for more info.
#' 
#' @section Testing:
#' If the package contains a \code{testthat} package suite, then selecting \code{test=TRUE} will
#' run a \code{\link[testthat]{test_package}} on the \code{package}.
#' 
#' @section Deploy:
#' if \code{deploy="hostname"}, then the newly built package bundle will be deployed, ie SCP'ed and
#'  installed to that host. Note that if you run updateR with \code{source=FALSE, binary=FALSE},
#'  then the existing package bundle will be deployed.\cr
#'  This handles a few scenarios:\cr
#' [1] local and remote usernames are identical, and you have setup password-less authentication, 
#'   then just set \code{deploy="hostname"}.\cr
#' [2] local and remote usernames differ, and you have setup password-less authentication, 
#'   then set \code{deploy="username@@hostname"}\cr
#' [3] you have to enter an SCP and SSH password, then\cr
#'   set \code{deploy="username@@hostname -p mypassword"}\cr
#' All of these scenario's will install the package to whereever you have rights to do so.
#' 
#' @section relibrary:
#' Once code has been updated, and INSTALLed, you can reload the package in the current \R
#' session. Note this can't be done from the commandline. This generally works well, but since
#' the R documentation files are cached in a lazyload db, it's impossible to update the 
#' documents in the same \R-session, and you will get this error:\cr
#' \dQuote{\code{Error in fetch(key) : internal error -3 in R_decompress1}}\cr
#' see \code{\link{reload}} for more info.
#'
#' @section TODO:
#' \describe{
#'   \item{devtools}{Investigate more of Hadley's \code{devtools} functions for build/check/install.}
#' }
#' 
#' @param package the name of the package to be updated. either quoted, or unquoted.
#' @param src.root the path to the root of the src files.
#' @param lib.loc where to install the library. Defaults to .libPaths()[1]
#' @param warn.conflicts see relibrary
#' @param roxygen logical: roxygenize the package (generate Rd files on the fly)?
#' @param check.source logical: \code{R CMD CHECK} the package prior to building it?
#' @param source logical: Build a source package? If \dQuote{TRUE}, and \dQuote{install=TRUE}, then this 
#'   is the package that will be installed.
#' @param binary logical: Build a binary package?
#' @param winbinary logical: Build a windows binary package?
#' @param install logical: \code{R CMD INSTALL} the package?
#' @param test logical: run a \code{testthat::\link[testthat]{test_package}} suite, 
#'   on the installed version of the package? Note this is done after this function
#'   is given the opportunity to install the package.
#' @param deploy either \code{NULL}, or the hostname to SCP and R CMD INSTALL the package bundle to. 
#'   see details.
#' @param no.vignettes logical: if \dQuote{TRUE}, turn off the creation of vignettes
#' @param no.manual logical: if \dQuote{TRUE}, turn off the creation of PDF manuals
#' @param no.docs logical: if \dQuote{TRUE}, turn off the creation of documentation
#' 
#' @export
#' @importFrom devtools reload
#' 
#' @seealso \code{\link{relibrary}}, \code{\link[roxygen2]{roxygenize}}, \code{\link[testthat]{test_package}}, \code{\link{.Rprofile}}, \code{\link{install.packages}}
#' @author Mark Cowley
#' 
#' @examples 
#' \dontrun{
#' # build a source package, and install.
#' updateR("updateR", "~/src/R")
#' updateR("updateR", "~/src/R", source=TRUE, install=TRUE)
#' # roxygenize, check.source, and then build a source package, and install.
#' updateR("updateR", "~/src/R", roxygen=TRUE, check.source=TRUE, source=TRUE, install=TRUE)
#' # roxygenize, check.source, and then build a source package, and install + test & deploy.
#' updateR("updateR", "~/src/R", roxygen=TRUE, check.source=TRUE, source=TRUE, install=TRUE, test=TRUE, deploy="enzo")
#' }
updateR <- function(package, src.root=getOption("src.root"), lib.loc=NULL, warn.conflicts = TRUE, 
	roxygen=FALSE,
	check.source=FALSE,
	source=TRUE,
	binary=FALSE,
	winbinary=FALSE, 
	no.vignettes=FALSE, no.manual=FALSE, no.docs=FALSE, 
	check.bundle=FALSE,
	install=TRUE,
	test=FALSE,
	deploy=NULL,
	no.examples=FALSE
	) {

	################################################################################
	# check args
	if ( !is.character(package) ) 
		package <- as.character(substitute(package))
	
	!is.null(src.root) || stop("You must set your src.root; either assign options(src.root='~/src/R') or pass in the a path into the 2nd argument\n")
	file.exists(src.root) || stop("You have not set src.root properly. Directory does not exist. Try setting options(src.root='~/src/R')")
	src.root <- path.expand(src.root)

	if( is.null(lib.loc) )
		lib.loc <- .libPaths()[1]
	if( !file.exists(lib.loc) ) {
		stop("lib.loc must be specified. Currently it's: ", shQuote(lib.loc))
	}
	lib.loc <- path.expand(lib.loc)
	################################################################################
	
	################################################################################
	# get the package path & check it's structure
	package.path <- file.path(src.root, package)
	if( !file.exists(package.path) ) {
		stop("Source code could not be found at", shQuote(package.path))
	}
	if( !all(file.exists(file.path(package.path, c("R", "DESCRIPTION")))) ) {
		stop("package does not contain an R folder, and/or DESCRIPTION file", shQuote(package.path))
	}
	################################################################################
	
	################################################################################
	# setup the arguments for running updateR.sh
	exe <- file.path(.path.package('updateR'), 'bin', 'updateR.sh')
	file.exists(exe) || stop("updateR.sh could not be found at", shQuote(exe))
	
	options <- sprintf("-l %s %s %s %s %s %s %s %s %s %s %s %s %s", 
						lib.loc, 
						ifelse(roxygen,          "-r", ""), 
						ifelse(check.source,     "-c", ""), 
						ifelse(source,           "-s", ""), 
						ifelse(binary,           "-b", ""), 
						ifelse(winbinary,        "-w", ""),
						ifelse(no.vignettes,     "-g", ""),
						ifelse(no.manual,        "-m", ""),
						ifelse(no.docs,          "-o", ""),
						ifelse(no.examples,      "-x", ""),
						ifelse(check.bundle,     "-C", ""), 
						ifelse(install,          "-i", ""),
						ifelse(test,             "-t", ""),
						ifelse(!is.null(deploy), "-d", deploy)
						)
	options <- trim(gsub(" +", " ", options))
	cmd <- sprintf('%s %s %s > /dev/stderr; echo $?', 
		exe, 
		options, 
		package.path
	) # the redirect allows $? to be stored in retval below.
	# cat(cmd, "\n")
	################################################################################
	
	################################################################################
	# run the script
	retval <- system(cmd, intern=TRUE)
	if( length(retval) == 0 )
		stop("updateR.sh command failed with no output.\n")
	retval <- as.numeric(retval)
	################################################################################
	
	################################################################################
	# reload the package in the current session
	if( install ) {
		if( retval == 0 ) {
			cat("Reloading", package.path, "\n")
			# relibrary(package, character.only=TRUE, warn.conflicts=warn.conflicts)
			reload(package.path)
		}
		else {
			cat("Library build failed. not reloaded.\n")
		}
	}
}
# CHANGELOG
# 2009-10-12: version 1
# 2009-12-02: major updates and made into a package.
# 2011-04-11: added flags to control --no-vignettes --no-manual --no-docs
# 2011-07-04: added install option
# 2012-02-21: added -x, --no-examples option.
# 2012-03-16: added a -t flag for running testthat::test_package
#             update the Date field in the DESCRIPTION file every time rox/build is run
#             uses devtools::reload to reload the package
# 2012-04-05: added -d flag for DEPLOY to enzo.
# 			  tidied code
#             added @importFrom
# 2012-05-02: reload needs the path to the package, not the package name. 
#             I think this coincides with an update made within R 2.14
# 2012-06-14: 
#            changed check -> check.source
#            added check.bundle
# 2012-07-10: changed deploy from logical to NULL, or [username@]hostname.
#             major documentation improvements.
#             updated updateR.sh to grab the PACKAGE_NAME from the DESCRIPTION file.
# 