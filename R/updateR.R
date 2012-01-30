#' A flexible R package updater.
#' 
#' This function can be used to roxygenize, check, build source/binary/windows binary, install
#' and reload R packages. 
#' Under-the-hood, it calls a shell script, which in turn calls various \dQuote{R CMD}
#' programs, like R CMD CHECK, R CMD roxygenize, R CMD BUILD, R CMD INSTALL.
#' This function assumes that you develop your R packages within a top level directory, eg ~/src/R
#' given by the \dQuote{src.root} option.
#' 
#' I tend to edit my source code in an external editor (TextMate), then will want to incorporate
#' those code changes into my current R session. A simple updateR("my.package", source=TRUE, install=TRUE),
#' causes the package to be built and then installed. Optional roxygenizing, and package checking
#' can occur, prior to package building.
#'
#' Roxygen is an inline documentation engine which builds \code{Rd} files from structured comment 
#' headers above each function. Roxygen creates \code{Rd} files, and updates the \code{NAMESPACE} and
#' the Date and Collates fields in the \code{DESCRIPTION} files, however it normally does this in a 
#' separate copy of the package.
#' \code{updateR} removes all previously existing \code{Rd} files & replaces them with new \code{Rd} 
#' files, and merges the changes made to \code{NAMESPACE} and \code{DESCRIPTION} into the package 
#' directory.
#' WARNING this will delete any files which are only encoded in the \code{Rd} format, and not in
#' roxygen comments. Try the \code{\link[Rd2roxygen]{Rd2roxygen}} package for converting from 
#' \code{Rd} to \code{roxygen} comments.
#'
#' @note
#' Currently, \code{\link{relibrary}} is unable to update the R documentation objects, giving 
#' internal errors.
#'
#' in case of epic fail: 
#' \verb{
#'    bash$ ~/src/R/updateR/inst/bin/updateR.sh ~/src/R/updateR
#'       R> relibrary("updateR")
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
#' @param no.vignettes logical: if \dQuote{TRUE}, turn off the creation of vignettes
#' @param no.manual logical: if \dQuote{TRUE}, turn off the creation of PDF manuals
#' @param no.docs if TRUE, logical: if \dQuote{TRUE}, turn off the creation of documentation
#' @export
#' @seealso \code{\link{relibrary}}, \code{\link[roxygen2]{roxygenize}}, \code{\link{.Rprofile}}, \code{\link{install.packages}}
#' @author Mark Cowley
#' @examples 
#' \dontrun{
#' # build a source package, and install.
#' updateR("updateR", "~/src/R")
#' updateR("updateR", "~/src/R", source=TRUE, install=TRUE)
#' # roxygenize, check, and then build a source package, and install.
#' updateR("updateR", "~/src/R", roxygen=TRUE, check=TRUE, source=TRUE, install=TRUE)
#' }
updateR <- function(package, src.root=getOption("src.root"), lib.loc=NULL, warn.conflicts = TRUE, 
	roxygen=FALSE,
	check=FALSE,
	source=TRUE,
	binary=FALSE,
	winbinary=FALSE, 
	install=TRUE,
	no.vignettes=FALSE, no.manual=FALSE, no.docs=FALSE) {
	if ( !is.character(package) ) 
		package <- as.character(substitute(package))
	
	if( is.null(src.root) )
		stop("You must set your src.root; either assign options(src.root='~/src/R') or pass in the a path into the 2nd argument\n")
	if( !file.exists(src.root) ) {
		stop("You have not set src.root properly. Directory does not exist. Try setting options(src.root='~/src/R')")
	}
	src.root <- path.expand(src.root)

	package.path <- file.path(src.root, package)
	if( !file.exists(package.path) ) {
		stop("Source code could not be found at", shQuote(package.path))
	}
	if( !all(file.exists(file.path(package.path, c("R", "DESCRIPTION")))) ) {
		stop("package does not contain an R folder, and/or DESCRIPTION file", shQuote(package.path))
	}
	
	if( is.null(lib.loc) )
		lib.loc <- .libPaths()[1]
	if( !file.exists(lib.loc) ) {
		stop("lib.loc must be specified. Currently it's: ", shQuote(lib.loc))
	}
	lib.loc <- path.expand(lib.loc)
	
	exe <- file.path(.path.package('updateR'), 'bin', 'updateR.sh')
	if( !file.exists(exe) ) {
		stop("updateR.sh could not be found at", shQuote(exe))
	}
	
	options <- sprintf("-l %s %s %s %s %s %s %s %s %s %s", 
						lib.loc, 
						ifelse(roxygen,      "-r", ""), 
						ifelse(check,        "-c", ""), 
						ifelse(source,       "-s", ""), 
						ifelse(binary,       "-b", ""), 
						ifelse(winbinary,    "-w", ""),
						ifelse(no.vignettes, "-g", ""),
						ifelse(no.manual,    "-m", ""),
						ifelse(no.docs,      "-d", ""),
						ifelse(install,      "-i", "")
						)
	options <- trim(gsub(" +", " ", options))
	cmd <- sprintf('%s %s %s > /dev/stderr; echo $?', 
		exe, 
		options, 
		package.path
	) # the redirect allows $? to be stored in retval below.
	# cat(cmd, "\n")

	retval <- system(cmd, intern=TRUE)
	if( length(retval) == 0 )
		stop("updateR.sh command failed with no output.\n")
	retval <- as.numeric(retval)

	if( install ) {
		if( retval == 0 ) {
			cat("Reloading", package, "\n")
			relibrary(package, character.only=TRUE, warn.conflicts=warn.conflicts)
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


# Update a package which is contained within a package bundle (ie a meta.package).
#
# DEPRECATED, since meta packages were dropped circa R 2.11
#
# I use the notation package and meta.package to denote the package to be updated, and the container itself.
#
# Parameters:
#	package: the package to be updated.
#	meta.package: the container/meta package which contains the package to be updated.
#	src.root: the path to the root of the src files.
#	lib.loc: where to install the library. Defaults to .libPaths()[1]
#	warn.conflicts: see relibrary
#	upload: upload the new tar.gz file to /pwbc/src/R
#
# Details:
#	the entire meta-package gets rebuilt, then the package of interest only gets reloaded.
#
# Mark Cowley, 2009-10-12
# 2009-12-02: major updates since this is now part of a package.
#
updateR.metapkg <- function(package, meta.package, src.root=getOption("src.root"), lib.loc=.libPaths()[1], 
	warn.conflicts = TRUE) {
	if( missing(package) || missing(meta.package) ) {
		stop("You must specify both the meta.package name, and the meta-meta.package name.\n")
	}
	
	if ( !is.character(package) ) 
		package <- as.character(substitute(package))
	if ( !is.character(meta.package) ) 
		meta.package <- as.character(substitute(meta.package))
	
	# exe <- file.path(.path.meta.package('updateR'), 'bin', 'updateR.sh')
	# cmd <- sprintf("%s %s > /dev/null && echo $?", exe, meta.package)
	# retval <- system(cmd, intern=TRUE)
	# retval <- as.numeric(retval)
	if( !file.exists(src.root) ) {
		stop("You have not set src.root properly. Directory does not exist. Try setting options(src.root='~/src/R')")
	}
	src.root <- path.expand(src.root)
	
	meta.package.path <- file.path(src.root, meta.package)
	if( !file.exists(meta.package.path) ) {
		stop("The meta-package code could not be found at", shQuote(meta.package.path))
	}
	package.path <- file.path(src.root, meta.package, package)
	if( !file.exists(package.path) ) {
		stop(sprintf("Package %s does not exist within the %s metapackage.\n", package, meta.package.path))
	}

	if( !file.exists(lib.loc) ) {
		stop("lib.loc must be specified. Currently it's: ", shQuote(lib.loc))
	}
	lib.loc <- path.expand(lib.loc)

	exe <- file.path(.path.package('updateR'), 'bin', 'updateR.sh')
	if( !file.exists(exe) ) {
		stop("updateR.sh could not be found at", shQuote(exe))
	}
	
	cmd <- sprintf('%s -l %s -s %s > /dev/null && echo $?', exe, lib.loc, meta.package.path)
	retval <- system(cmd, intern=TRUE)
	if( length(retval) == 0 )
		stop("The updateR.sh command failed with no output.\n")
	retval <- as.numeric(retval)

	if( retval == 0 ) {
		cat("Reloading", package, "\n")
		relibrary(package, character.only=TRUE, warn.conflicts=warn.conflicts)
	}
	else {
		cat("Library build failed. not reloaded.\n")
	}
}
