#' Unload a library.
#' 
#' Unload a library which has already been \code{load}ed. Adapted from \code{\link{relibrary}}
#' @param package Name or character string giving the name of a package.
#' @param character.only A logical indicating whether \sQuote{package} can be
#'     assumed to be character strings. Default value is \code{FALSE}.
#' @param \dots Currently ignored.
#' @author Mark Cowley, based on \code{\link{relibrary}} from Henrik Bengtsson, \email{henrikb@@braju.com},
#'     \url{http://www.braju.com/R/}
#' @seealso See \code{\link[base]{library}} \code{\link{relibrary}}.
#' @return nothing.
#' @examples
#' \dontrun{
#' unlibrary(updateR)
#' }
#' @export
#' 
unlibrary <- function(package, character.only=FALSE, ...) {
	if (!character.only) {
		package <- as.character(substitute(package))
		character.only <- TRUE
	}

	if (paste("package:",package,sep="") %in% search()){
		detach(pos=match(paste("package:",package,sep=""), search()), unload=TRUE, force=TRUE)
	}
	
	# if( package %in% loadedNamespaces() ) {
	# 	unloadNamespace(package)
	# }
	# else if (paste("package:",package,sep="") %in% search()){
	# 	detach(pos=match(paste("package:",package,sep=""), search()), unload=TRUE, force=TRUE)
	# }
}
############################################################################
# HISTORY:
# 2012-01-24
# - simplified & using detach
# - This still doesn't get around the dreaded error: 
#   "Error in fetch(key) : internal error -3 in R_decompress1"
# - According to Ripley's reply, it's impossible to do so:
#   (http://r.789695.n4.nabble.com/Unable-to-reload-Rdoc-td4333063.html)
#   "This is simply not supported.  Lazy-load databases are cached, and you
#   cannot expect to change them during the R session once they have been used."
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

