#' List my R packages which all sit inside src.root
#'
#' @param src.root The root folder containing all R package code.
#' @param maxdepth do you only want to look into the folder 1 below src.root, or further? defaults to 1.
#' @param only.installed if TRUE, then only return packages which are also installed. if FALSE, then all candidate packages are returned.
#' 
#' @return A vector of package names
#' @author Mark Cowley, 2011-04-07
#' @export
#' @examples
#' list.my.packages()
#' list.my.packages(only.installed=FALSE)
list.my.packages <- function(src.root=getOption("src.root"), maxdepth=1, only.installed=TRUE) {
	!is.null(src.root) || stop("src.root must be a valid path")
	src.root <- path.expand(src.root)
	cmd <- sprintf("find %s -name DESCRIPTION -maxdepth %d -print", shQuote(src.root), maxdepth+1)
	# cat(cmd, "\n")
	packages <- system(cmd, intern=TRUE)
	packages <- sub("/DESCRIPTION", "", packages)
	packages <- packages[!grepl("\\.Rcheck", packages)]
	packages <- sub(paste(src.root,"/?", sep=""), "", packages)
	if( only.installed ) packages <- intersect(packages, installed.packages()[,"Package"])
	
	packages
}
# debug( list.my.packages )
# list.my.packages()
