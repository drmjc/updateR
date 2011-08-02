#' Unload a library.
#' Unload a library which has already been \code{load}ed. Adapted from \code{\link{relibrary}}
#' @param package Name or character string giving the name of a package.
#' @param character.only A logical indicating whether \sQuote{package} can be
#'   assumed to be character strings. Default value is \code{FALSE}.
#' @param warn.conflicts If \code{TRUE}, warnings are printed about conflicts
#'   from reattaching of the package, unless that package contains an object
#'   \code{.conflicts.OK}. Default value is \code{FALSE}.
#' @param \dots Currently ignored.
#' @author Mark Cowley, based on \code{\link{relibrary}} from Henrik Bengtsson, \email{henrikb@@braju.com},
#'   \url{http://www.braju.com/R/}
#' @seealso See \code{\link[base]{library}} \code{\link{relibrary}}.
#' @return nothing.
#' @examples
#' \dontrun{
#' unlibrary(updateR)
#' }
#' @export
#' 
unlibrary <- function(package, character.only=FALSE, warn.conflicts=TRUE, ...) {
  if (!character.only)
    package <- as.character(substitute(package));

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
    .Internal(detach(pos));
  }
}
############################################################################
# HISTORY:
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

