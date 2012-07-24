#' list the function names defined in a source file
#'
#' @section TODO:
#' this only works with "<- function". It doesn't work with "= function",
#' nor with S4 functions.
#' 
#' @param f the path to a .R source file
#' @return a character vector of function names
#' @author Mark Cowley, 2011-11-10
#' 
#' @export
#' @importFrom stringr str_replace
#' 
#' @examples
#' # debugonce(functions.in.source.file)
#' functions.in.source.file("~/src/R/metaGSEA/R/export.gsea.gct.R")
#' functions.in.source.file("~/src/R/mjcdev/R/igraph.utils.R")
#' # functions.in.source.file("/misc/ICGCPancreas/PINA/bin/GeneSetCollection-methods.R")
functions.in.source.file <- function(f) {
	!missing(f) && (length(f)==1) && file.exists(f) && grepl(".*[Rr]$", f) || stop("f should be a path to a source file ending in .R or .r")
	
	res <- character()
	txt <- readLines(f)
	lines <- grep("^([[:alpha:]._][[:alnum:]._]+)[[:space:]]+<-[[:space:]]+function", txt, value=TRUE)
	if( length(lines) > 0 )
		res <- c(res, str_replace(lines, "^([[:alpha:]._][[:alnum:]._]+)[[:space:]]+<-[[:space:]]+function.*", "\\1"))
	
	# any S4 methods?
	lines <- grep("^setMethod", txt, value=TRUE)
	if( length(lines) > 0 ) {
		lines <- str_replace(lines, 'setMethod[[:space:]]*\\([[:space:]]*[\'"]([[:alnum:]]+).*', "\\1")
		res <- c(res, lines)
	}
	res
}
