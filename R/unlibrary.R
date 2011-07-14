## Function to unload a library.
## adapted from relibrary
##
## Downloaded from:CRAN pages title "RE: [R] easier way to update packages i'm developing?"
##
unlibrary <- function(package, character.only=FALSE, warn.conflicts=T, ...) {
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

} # unlibrary

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

