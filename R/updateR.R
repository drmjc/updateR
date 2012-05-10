#' A flexible R package updater.
#' 
#' This function can be used to roxygenize, check, build source/binary/windows binary, install,
#' reload, test and deploy R packages. 
#' Under-the-hood, it calls a shell script, which in turn calls various \dQuote{R CMD}
#' programs, like R CMD {CHECK,BUILDINSTALL}, in addition to some others, including
#' roxygen, testthat.
#' This function assumes that you develop your R packages within a top level directory, eg ~/src/R
#' given by the \code{options("src.root")} option.
#' 
#' I tend to edit my source code in an external editor (TextMate), then will want to incorporate
#' those code changes into my current R session. A simple updateR("my.package", source=TRUE, install=TRUE),
#' causes the package to be built and then installed. 
#' Optional roxygenizing & package path checking can be run prior to package building;\cr
#' source or binary package bundle building;\cr
#' installation, reloading the package in the current session;\cr
#' followed by testing via Hadley Wickham's \code{testthat} and deployment to enzo after installation.
#'
#' @section Roxygen2:
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
#' @section Checking:
#' \code{R CMD CHECK} is run on the package folder, not the package bundle. It's on my todo list to
#' supporte \code{CHECK}ing of the tar.gz, possibly with the \code{--as-cran} option.
#' 
#' @section Testing:
#' If the package contains a testthat package suite, then selecting \code{test=TRUE} will
#' run a \code{\link[testthat]{test_package}} on the \code{package}.
#' 
#' @section Deploy:
#' if \code{deploy=TRUE}, then the newly built package bundle will be deployed, ie copied and
#'  installed to enzo. Note that if you run updateR with \code{source=FALSE, binary=FALSE},
#'  then the existing package bundle will be deployed. This assumes that you have setup
#'  password-less authentication on the target host, and the username is the same as on the
#'  submission machine.
#' 
#' @note
#' Currently, \code{\link{relibrary}} is unable to update the R documentation objects, giving 
#' internal errors.
#'
#' @section in case of epic fail: 
#' \verb{
#'    bash$ ~/src/R/updateR/inst/bin/updateR.sh ~/src/R/updateR
#'       R> relibrary("updateR")
#' }
#'
#' @section TODO:
#' \describe{
#'   \item{devtools}{Investigate more of the \code{devtools} functions for build/check/install.}
#'   \item{Checking bundle}{Run R CMD CHECK --as-cran on the package bundle. Why? 'cos that's what\cr
#' CRAN runs their checks on -- not the folder which may mask errors due to the way i've setup my
#' environment.}
#' }
#' 
#' @param package the name of the package to be updated. either quoted, or unquoted.
#' @param src.root the path to the root of the src files.
#' @param lib.loc where to install the library. Defaults to .libPaths()[1]
#' @param warn.conflicts see relibrary
#' @param roxygen logical: roxygenize the package (generate Rd files on the fly)?
#' @param check logical: \code{R CMD CHECK} the package prior to building it?
#' @param source logical: Build a source package? If \dQuote{TRUE}, and \dQuote{install=TRUE}, then this 
#'   is the package that will be installed.
#' @param binary logical: Build a binary package?
#' @param winbinary logical: Build a windows binary package?
#' @param install logical: \code{R CMD INSTALL} the package?
#' @param test logical: run a \code{testthat::\link[testthat]{test_package}} suite, 
#'   on the installed version of the package? Note this is done after this function
#'   is given the opportunity to install the package.
#' @param deploy logical: if \code{TRUE}, then deploy the \code{package_VERSION.tar.gz} to\cr
#'   enzo. Note if you don't build a new package (ie \code{source=FALSE}, and \code{binary=FALSE}),
#'   then this will deploy the existing version of the package bundle, else the newly
#'   created package bundle will be deployed.
#' @param no.vignettes logical: if \dQuote{TRUE}, turn off the creation of vignettes
#' @param no.manual logical: if \dQuote{TRUE}, turn off the creation of PDF manuals
#' @param no.docs logical: if \dQuote{TRUE}, turn off the creation of documentation
#' 
#' @export
#' @importFrom devtools reload
#' 
#' @seealso \code{\link{relibrary}}, \code{\link[roxygen2]{roxygenize}}, \code{\link{.Rprofile}}, \code{\link{install.packages}}
#' @author Mark Cowley
#' 
#' @examples 
#' \dontrun{
#' # build a source package, and install.
#' updateR("updateR", "~/src/R")
#' updateR("updateR", "~/src/R", source=TRUE, install=TRUE)
#' # roxygenize, check, and then build a source package, and install.
#' updateR("updateR", "~/src/R", roxygen=TRUE, check=TRUE, source=TRUE, install=TRUE)
#' # roxygenize, check, and then build a source package, and install + test & deploy.
#' updateR("updateR", "~/src/R", roxygen=TRUE, check=TRUE, source=TRUE, install=TRUE, test=TRUE, deploy=TRUE)
#' }
updateR <- function(package, src.root=getOption("src.root"), lib.loc=NULL, warn.conflicts = TRUE, 
	roxygen=FALSE,
	check=FALSE,
	source=TRUE,
	binary=FALSE,
	winbinary=FALSE, 
	install=TRUE,
	test=FALSE,
	deploy=FALSE,
	no.vignettes=FALSE, no.manual=FALSE, no.docs=FALSE, no.examples=FALSE) {

	################################################################################
	# check args
	if ( !is.character(package) ) 
		package <- as.character(substitute(package))
	
	if( is.null(src.root) )
		stop("You must set your src.root; either assign options(src.root='~/src/R') or pass in the a path into the 2nd argument\n")
	if( !file.exists(src.root) ) {
		stop("You have not set src.root properly. Directory does not exist. Try setting options(src.root='~/src/R')")
	}
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
						ifelse(roxygen,      "-r", ""), 
						ifelse(check,        "-c", ""), 
						ifelse(source,       "-s", ""), 
						ifelse(binary,       "-b", ""), 
						ifelse(winbinary,    "-w", ""),
						ifelse(no.vignettes, "-g", ""),
						ifelse(no.manual,    "-m", ""),
						ifelse(no.docs,      "-o", ""),
						ifelse(no.examples,  "-x", ""),
						ifelse(install,      "-i", ""),
						ifelse(test,         "-t", ""),
						ifelse(deploy,       "-d", "")
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

# # Update a package which is contained within a package bundle (ie a meta.package).
# #
# # DEPRECATED, since meta packages were dropped circa R 2.11
# #
# # I use the notation package and meta.package to denote the package to be updated, and the container itself.
# #
# # Parameters:
# #	package: the package to be updated.
# #	meta.package: the container/meta package which contains the package to be updated.
# #	src.root: the path to the root of the src files.
# #	lib.loc: where to install the library. Defaults to .libPaths()[1]
# #	warn.conflicts: see relibrary
# #	upload: upload the new tar.gz file to /pwbc/src/R
# #
# # Details:
# #	the entire meta-package gets rebuilt, then the package of interest only gets reloaded.
# #
# # Mark Cowley, 2009-10-12
# # 2009-12-02: major updates since this is now part of a package.
# #
# updateR.metapkg <- function(package, meta.package, src.root=getOption("src.root"), lib.loc=.libPaths()[1], 
# 	warn.conflicts = TRUE) {
# 	if( missing(package) || missing(meta.package) ) {
# 		stop("You must specify both the meta.package name, and the meta-meta.package name.\n")
# 	}
# 	
# 	if ( !is.character(package) ) 
# 		package <- as.character(substitute(package))
# 	if ( !is.character(meta.package) ) 
# 		meta.package <- as.character(substitute(meta.package))
# 	
# 	# exe <- file.path(.path.meta.package('updateR'), 'bin', 'updateR.sh')
# 	# cmd <- sprintf("%s %s > /dev/null && echo $?", exe, meta.package)
# 	# retval <- system(cmd, intern=TRUE)
# 	# retval <- as.numeric(retval)
# 	if( !file.exists(src.root) ) {
# 		stop("You have not set src.root properly. Directory does not exist. Try setting options(src.root='~/src/R')")
# 	}
# 	src.root <- path.expand(src.root)
# 	
# 	meta.package.path <- file.path(src.root, meta.package)
# 	if( !file.exists(meta.package.path) ) {
# 		stop("The meta-package code could not be found at", shQuote(meta.package.path))
# 	}
# 	package.path <- file.path(src.root, meta.package, package)
# 	if( !file.exists(package.path) ) {
# 		stop(sprintf("Package %s does not exist within the %s metapackage.\n", package, meta.package.path))
# 	}
# 
# 	if( !file.exists(lib.loc) ) {
# 		stop("lib.loc must be specified. Currently it's: ", shQuote(lib.loc))
# 	}
# 	lib.loc <- path.expand(lib.loc)
# 
# 	exe <- file.path(.path.package('updateR'), 'bin', 'updateR.sh')
# 	if( !file.exists(exe) ) {
# 		stop("updateR.sh could not be found at", shQuote(exe))
# 	}
# 	
# 	cmd <- sprintf('%s -l %s -s %s > /dev/null && echo $?', exe, lib.loc, meta.package.path)
# 	retval <- system(cmd, intern=TRUE)
# 	if( length(retval) == 0 )
# 		stop("The updateR.sh command failed with no output.\n")
# 	retval <- as.numeric(retval)
# 
# 	if( retval == 0 ) {
# 		cat("Reloading", package, "\n")
# 		relibrary(package, character.only=TRUE, warn.conflicts=warn.conflicts)
# 	}
# 	else {
# 		cat("Library build failed. not reloaded.\n")
# 	}
# }
