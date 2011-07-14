#' Reloads a package
#' Reloads a package. This function works exactly the same as
#' \code{\link[base]{library}}, \emph{except} that it is reloads the package if
#' it already loaded. This is useful for developers. For more information see
#' \code{\link[base]{library}}.
#' 
#' While relibrary is reloading a package the option \code{relibrary} will be
#' set to name of the package currently reloaded. This can be useful if the
#' package to be reloaded would like save away data until it is loaded again.
#' 
#' As of R 2.12, (and perhaps before) reloading of documentation files doesn't
#' appear to be working. If you change doc files, then for the time being, the
#' whole R session needs restarting. Relibrary does still work OK for updating
#' source code changes.
#'
#' @note
#' Currently, if the roxygen comments, or Rd files change, then relibrary is 
#' unable to update the man files if the package has already been loaded.
#' The following error is produced when a man page is requested:
#' \code{Error in tools:::fetchRdDB(RdDB, basename(file)) : 
#'  internal error -3 in R_decompress1}
#'
#' @references
#' \url{https://stat.ethz.ch/pipermail/r-help/2002-January/018006.html}
#'
#' @param package Name or character string giving the name of a package.
#' @param character.only A logical indicating whether \code{package} can be
#'   assumed to be character strings. Default value is \code{FALSE}.
#' @param warn.conflicts If \code{TRUE}, warnings are printed about conflicts
#'   from reattaching of the package, unless that package contains an object
#'   \code{.conflicts.OK}. Default value is \code{FALSE}.
#' @param force logical: See \code{\link{detach}}.
#' @param ... Any other arguments that \code{\link[base]{library}} accepts.
#' @author Henrik Bengtsson, \email{henrikb@@braju.com},
#'   \url{http://www.braju.com/R/}, updated by Mark Cowley.
#' @seealso See \code{\link[base]{library}}.
#' @export
relibrary <- function(package, character.only=FALSE, warn.conflicts=TRUE, unload=TRUE, force=FALSE, ...) {
	if (!character.only)
		package <- as.character(substitute(package));

	options.relibrary <- options(relibrary=package)[[1]];

	# with namespaces, you can't detach a package if other packages
	# depend on it. These dependencies will need detaching, then reattaching.
	# @TODO - respect the original order of those dependencies in search()
	# @TODO - make this recursive, since these dependencies may have their own dependencies
	loaded.dependencies <- function(package) {
		# which packages depend upon this package that are also loaded
		lp <- grep("package", search(), value=T)
		lp <- sub("package:", "", lp)
		if( ! package %in% lp ) return( character(0) )
		ip <- as.data.frame(installed.packages(), stringsAsFactors=FALSE)
		ip <- subset(ip, package %in% lp)
		depends <- strsplit(ip$Depends, ", ")
		names(depends) <- ip$Package
		idx <- which(sapply(depends, function(x) package %in% x))
		pkg.dependencies <- names(idx)
		pkg.dependencies
	}
	
	pkg.dependencies <- loaded.dependencies(package)
	if( length(pkg.dependencies) > 0 ) {
		cat("Detaching dependencies: ")
		print(pkg.dependencies)
		for( i in 1:length(pkg.dependencies) ) {
			unlibrary(pkg.dependencies[i], character.only=TRUE)
		}
	}
	
	# If package is already attached, then detach it first.
	pkgName <- paste(sep="", "package:", package);
	pos <- match(pkgName, search());
	if (!is.na(pos)) {
		# If there exists a function .Last.lib() in the package call it first!
		if (exists(".Last.lib", where=pos, inherits=FALSE)) {
			.Last.lib <- get(".Last.lib", pos=pos, inherits=FALSE);
			if (is.function(.Last.lib)) {
				libpath <- attr(pos.to.env(pos), "path");
				if (!is.null(libpath))
					try(.Last.lib(libpath));
			}
		}
		# ...then remove the package.
		# I need a version which handles packages with namespaces....
		# # V1.
		# .Internal(detach(pos));
		# # V2.
		# detach(pos=pos, unload=TRUE)
		# # V3.
		# tryCatch(
		# 	detach(pos=pos, unload=TRUE), 
		# 	.Internal(detach(pos)),
		# 	silent=TRUE)
		# # V4. - use .path.package and packageHasNamespace, both from base
		# if( packageHasNamespace(package, dirname(.path.package(package))) ) {
		# 	cat(sprintf("%s has a namespace...\n", package))
		# 	detach(pos=pos, unload=TRUE, force=force)
		# }
		# else {
		# 	cat(sprintf("%s lacks a namespace...\n", package))
		# 	.Internal(detach(pos), force=force)
		# }
		# V5. as V4, but as of R 2.12, .Internal(detach) only accepts 1 arg.
		hasNamespace <- packageHasNamespace(package, dirname(.path.package(package)))
		cat(sprintf("%s %s a namespace...\n", package, ifelse(hasNamespace, "has", "lacks")))
		if(hasNamespace) unloadNamespace(package)
		detach(pos=pos, unload=unload, force=force)
	}

	library(package=package, character.only=TRUE, warn.conflicts=warn.conflicts, ...);
	options(relibrary=options.relibrary);
	if( length(pkg.dependencies) > 0 ) {
		cat("Reattaching dependencies: ")
		print(pkg.dependencies)
		for( i in 1:length(pkg.dependencies) )
			library(pkg.dependencies[i], character.only=TRUE)
	}
}

############################################################################
# HISTORY:
# 2011-07-04:
#  MJC - reloading of changed R doc files is failing.
# 2009-10-12
#  MJC either my shift to R 2.9.2, or use of namespaces meant that the code failed. 
#  detach(..., unload=TRUE) was necessary to force the correct behaviour.
# 2010-10-13
#  MJC as of R 2.11, you can't unload a package if it's a dependency of another
#  loaded package. Code update to unload dependencies, and then reload them after a 
#  successful update
# 2001-08-06
# * Now relibrary will set the option "relibrary" to the name of the package
# it is currently reloading. This option can be used by the package itself
# in its .First.lib and .Last.lib for instance in case it will store
# something until it is loaded again.
# 2001-08-03
# * Made relibrary by default not warning for conflicts.
# 2001-07-05
# * It is NOT possible to use UseMethod("relibrary", obj, ...).
# 2001-05-03
# * Created.
############################################################################

