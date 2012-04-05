#' Reloads a package
#' 
#' Reloads a package. This function works exactly the same as
#' \code{\link[base]{library}}, \emph{except} that it is reloads the package if
#' it already loaded. This is useful for developers. For more information see
#' \code{\link[base]{library}}. Hadley Wickham has since written the \code{devtools}
#' package which has a very similar \code{reload} function.
#' 
#' While \code{relibrary} is in the process of reloading a package the 
#' \code{options(relibrary)} will be
#' set to name of the package currently being reloaded. This can be useful if the
#' package to be reloaded would like save away data until it is loaded again.
#' 
#' As of R 2.12, (and perhaps before) reloading of documentation files doesn't
#' appear to be working. If you change doc files, then for the time being, the
#' whole R session needs restarting.Relibrary does still work OK for updating
#' source code changes. 
#' 
#' @note Reloading packages after making changes to documentation\cr
#' If you edit an Rd file, rebuild, reinstall & reload the package in an active R session, you
#' will get this error:\cr
#' \dQuote{\code{Error in fetch(key) : internal error -3 in R_decompress1}}
#' I've confirmed with Rdevel (2012-01-27) that it is impossible to work around
#' this error... you have to restart your R session.\cr
#' Prof Ripley: \dQuote{This is simply not supported.  Lazy-load databases are cached, and you
#' cannot expect to change them during the R session once they have been used.}
#' More info, including 3 suggestions here:
#' \url{http://r.789695.n4.nabble.com/Unable-to-reload-Rdoc-td4333063.html}
#' Hadley's \code{devtools::check_Rd} function will render an Rd file without
#' having to relaod it.
#'
#' @references
#' \url{https://stat.ethz.ch/pipermail/r-help/2002-January/018006.html}
#' \url{http://r.789695.n4.nabble.com/Unable-to-reload-Rdoc-td4333063.html}
#'
#' @param package Name or character string giving the name of a package.
#' @param character.only A logical indicating whether \code{package} can be
#'   assumed to be character strings. Default value is \code{FALSE}.
#' @param warn.conflicts If \code{TRUE}, warnings are printed about conflicts
#'   from reattaching of the package, unless that package contains an object
#'   \code{.conflicts.OK}. Default value is \code{FALSE}.
#' @param \dots Any other arguments that \code{\link[base]{library}} accepts.
#' @author Henrik Bengtsson, \email{henrikb@@braju.com},
#'   \url{http://www.braju.com/R/}, updated by Mark Cowley.
#' @seealso See \code{\link[base]{library}} \code{\link{unlibrary}}
#' @export
relibrary <- function(package, character.only=FALSE, warn.conflicts=TRUE, ...) {
	if (!character.only)
		package <- as.character(substitute(package));

	options.relibrary <- options(relibrary=package)[[1]];

	loaded.dependencies <- function(pkg) {
		require(tools) || stop("required package 'tools' is not installed")

		ns <- loadedNamespaces()
		ns.deps <- sapply(ns, function(x) pkgDepends(x)$Depends)
		idx <- sapply(ns.deps, function(x) pkg %in% x)
		ns.deps <- subset(ns.deps, idx)
		if( length(ns.deps) > 0 ) {
			res <- c(unlist(sapply(names(ns.deps), loaded.dependencies)), names(ns.deps))
		}
		else {
			res <- character(0)
		}
		unique(res)
	}
	# loaded.dependencies("pwbc")
	# loaded.dependencies("metaGSEA")
		
	# If package is already attached, then detach its loaded dependencies, then itself
	pkgName <- paste(sep="", "package:", package);
	pos <- match(pkgName, search());
	pkg.dependencies <- NULL
	if (!is.na(pos)) {
		pkg.dependencies <- loaded.dependencies(package)
		if( length(pkg.dependencies) > 0 ) {
			cat("Detaching dependencies: ")
			print(pkg.dependencies)
			for( i in 1:length(pkg.dependencies) ) {
				unlibrary(pkg.dependencies[i], character.only=TRUE)
			}
		}
		
		hasNamespace <- packageHasNamespace(package, dirname(.path.package(package)))
		cat(sprintf("%s %s a namespace...\n", package, ifelse(hasNamespace, "has", "lacks")))
		unlibrary(package, character.only=TRUE)
	}

	if( length(pkg.dependencies) > 0 ) {
		cat("Reattaching dependencies: ")
		print(pkg.dependencies)
		for( i in 1:length(pkg.dependencies) )
			library(pkg.dependencies[i], character.only=TRUE)
	}
	library(package=package, character.only=TRUE, warn.conflicts=warn.conflicts, ...)
	options(relibrary=options.relibrary)
}
############################################################################
# HISTORY:
# 2012-02-29:
# - dropped the no longer used unload and force arguments (unlibrary sets both to TRUE)
# 2012-01-24:
# - wrote recursive version of loaded.dependencies.
# - unload packages using the updated unlibrary
# - improved doc & @note regarding the Rd reloading error.
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

