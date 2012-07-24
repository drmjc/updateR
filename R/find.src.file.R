#' Find the source file for a given function.
#'
#' Given a directory of R packages under development (under src.root), or a vector of
#' R files, given by src.files, determine the name of the source file which func
#' was defined within.
#'
#' @param func a function name, or function's code
#' @param src.root The parent folder of source code
#' @param src.files an optional vector of filenames to search within
#' @param exclude.patterns vector of patterns passed to grep for files to exclude
#' @param unique logical: If \code{TRUE} then at most 1 file path is returned; if \code{FALSE}, all matching
#'   filepaths are returned
#' @param exclude.symlinks logical: if \code{TRUE}, then only report files which are not
#'  symbolic links.
#' 
#' @return a vector of file paths
#' 
#' @author Mark Cowley
#' @export
#' @importFrom mjcbase file.issymlink
find.src.file <- function(func, src.root=getOption("src.root"), src.files=NULL, exclude.patterns=c("~", ".Rcheck", ".git", ".svn"), unique=TRUE, exclude.symlinks=TRUE) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	# if this is a setter, eg 'colclasses<-' then the func name is quoted
	if(grepl("<-", func)) func <- paste("'", func, "'", sep = "")
	
	pattern <- paste("^", func, " *<- *function", sep="")

	# locations can be a combination of a directory specified by src.root, and/or a vector of named files in src.files.
	# grep and ack don't mind mixes of trailing dirs and files.
	locations <- c(src.root, src.files)
	!is.null(locations) || stop("Must specify at least one of src.root and src.files.")
	locations <- shQuote(normalizePath(locations))
	locations <- paste(locations, collapse=" ")

	cmd <- paste("grep -l -R", shQuote(pattern), locations)
	# ack is a MUCH faster version of grep & should be used if its available.
	if(nzchar(Sys.which("ack"))) cmd <- sub("^grep", "ack", cmd)
	
	suppressWarnings( files <- system(cmd, intern=TRUE) ) # if no file is found, then a warning is generated.
	
	if( length(files) == 0 ) {
		files <- NA
	}
	else {
		for(pattern in exclude.patterns) {
			if( any(grepl(pattern, files)) )
				files <- files[-grep(pattern, files)]
		}
	}

	if( exclude.symlinks ) {
		symlinks <- file.issymlink(files)
		files <- files[!symlinks]
	}

	if( unique && length(files) > 1 ) {
		files <- files[1]
	}
	
	files
}
# CHANGELOG
# 26/2/07: v1
# 2011-04-07: updated to exclude a bunch of patterns
# 2011-04-19: updated to locate setters, eg 'colclasses<-'
# 2011-04-20: added unique parameter.
# 2011-04-20: added support for ack.
# 2011-06-21: added src.files arg
# 2012-07-24: added exclude.symlinks code

#' Find the source file for a given function.
#' This is a wrapper around \code{\link{find.src.file}}
#' @seealso \code{\link{find.src.file}}
#' @param func a function name, or function's code
#' @param src.root The parent folder of source code
#' @param src.files an optional vector of filenames to search within
#' @param exclude.patterns vector of patterns passed to grep for files to exclude
#' @param unique logical: If TRUE then at most 1 file path is returned; if FALSE, all matching
#'   filepaths are returned
#' @return a vector of file paths, 
#' @author Mark Cowley
#' @export
which.src.file <- function(func, src.root=getOption("src.root"), src.files=NULL, exclude.patterns=c("~", ".Rcheck", ".git", ".svn"), unique=TRUE) {
	find.src.file(func, src.root, src.files=src.files, exclude.patterns=exclude.patterns, unique=unique)
}
