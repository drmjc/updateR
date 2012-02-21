#' Detach all user-loaded packages
#'
#' @param keep a vector of packages to not detach. Defaults to the packages that 
#'  are automatically loaded upon system start, as well as \code{Autoloads}, 
#'  \code{base}, and the \code{.GlobalEnv}
#' @return invisibly returns the vector of package names that were detached
#' @seealso \code{\link{detach}} \code{\link{options}}
#' @author Mark Cowley, 2011-10-19
#' @export
#' @examples
#' \dontrun{
#' detach.all()
#' }
detach.all <- function(keep=getOption("defaultPackages")) {
	loaded.pkgs <- basename(searchpaths())
	loaded.pkgs <- loaded.pkgs[! loaded.pkgs %in% c(".GlobalEnv", "Autoloads", "base", keep)]
	if( length(loaded.pkgs) > 0 ) {
		loaded.pkgs <- paste("package:", loaded.pkgs, sep="")
		for(pkg in loaded.pkgs) {
			detach(pos=match(pkg, basename(search()))[1])
			# pos <- match(basename(searchpaths()))
			# detach(pkg, character.only=TRUE)
		}
	}
	
	invisible(loaded.pkgs)
}
