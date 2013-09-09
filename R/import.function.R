#' Search for & import the function definition from within a source file.
#'
#' I usually write detailed comment headers before functions in source code files.
#' scenarios:
#'	1. function header preceded by comment block preceded by a blank line, or top of file
#'	2. function header preceded by (blank line or top of file) -> no comment has been written.
#'	
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @param src.files an optional vector of filenames to search within
#' @param import.header search for & import the header comments. TRUE/FALSE
#' @param exclude.patterns vector of patterns passed to grep for files to exclude
#' 
#' @return This code extracts the src code, and the header, or returns ""
#' 
#' @export
#' @author Mark Cowley, 2011-04-07
#' 
import.function <- function(func, src.root=getOption("src.root"), src.files=NULL, import.header=TRUE, exclude.patterns=c("~", ".Rcheck", ".git", ".svn")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))

	f <- find.src.file(func, src.root, src.files, exclude.patterns=exclude.patterns)
	if( length(f) == 0 || (length(f)==1 && is.na(f))) {
		stop("No src code containing func was found.\n")
	} else if( length(f) > 1 ) {
		warning("Found multiple files that match this function signature\n")
		f <- f[1]
	}
	import.function.from.src.file(func=func, f=f, import.header=import.header)
}


#' Import the function definition from within a source file.
#'
#' I usually write detailed comment headers before functions in source code files.
#' scenarios:
#'	1. function header preceded by comment block preceded by a blank line, or top of file
#'	2. function header preceded by (blank line or top of file) -> no comment has been written.
#'	
#' @param func a function name, or the function's body. eg \code{plot} or \dQuote{\code{plot}}
#' @param f The path to the file containing the function.
#' @param import.header search for & import the header comments. TRUE/FALSE
#' @return This code extracts the src code, and the header, or returns ""
#' @export
#' @author Mark Cowley, 2011-06-21
#'
import.function.from.src.file <- function(func, f, import.header=TRUE) {
	!missing(func) || stop("must specify a function")
	(!missing(f) && (length(f) == 1) && !is.na(f) && file.exists(f)) || stop("must specify 1 input file")
	
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	
	# if this is a setter, eg 'colclasses<-' then the func name is quoted
	if(grepl("<-", func)) func <- paste("'", func, "'", sep = "")
	
	src <- readLines(f, warn=FALSE)

	pattern <- paste("^[[:space:]]*", func, "[[:space:]]*<-[[:space:]]*function", sep="")
	func.start.line <- grep(pattern, src)
	comment.lines   <- grep("^#", src)
	blank.lines     <- c(0,which(nchar(src) == 0))
	
	if( length(func.start.line) == 0 ) {
		stop(sprintf("Could not find the function '%s' within '%s'", func, file))
		return("")
	}
	else if( length(func.start.line) > 1 ) {
		warning("Found multiple entries in the same src file that match the code signature")
		func.start.line <- func.start.line[1]
	}
	comment.header <- ""
	if ( import.header && ((func.start.line - 1) %in% comment.lines) ) {
		# Then there is a comment line preceeding this function header.
		blanks.above <- blank.lines[blank.lines < func.start.line]
		header.start <- blanks.above[length(blanks.above)] + 1
		header.stop <- func.start.line - 1
		# if( ! src[header.start] %in% comment.lines ) {
		# 	warning("I'd expect a comment line to be after a blank line...\n")
		# }
		comment.header <- src[c(header.start:header.stop)]
	}
	
	# now get the src code:
	func.stop.lines <- c(grep("^}", src), length(src)) # in case there's no lines matching ^} , keep reading to the end.
	func.stop.line <- func.stop.lines[func.stop.lines>func.start.line][1]
	src.code <- src[func.start.line:func.stop.line]
	if( import.header ) {
		src.code <- c(comment.header, src.code)
	}
	
	return(src.code)
}


#' Search for & import the function header from within a source file.
#'
#' I usually write detailed comment headers before functions in source code files.
#' scenarios:
#'	1. function header preceded by comment block preceded by (blank line, or top of file)
#'	2. function header preceded by (blank line or top of file) -> no comment has been written.
#'	
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @param src.files an optional vector of filenames to search within
#' @param strip.comment.char logical: if TRUE, strip the leading comment characters. 
#' @param exclude.patterns vector of patterns passed to grep for files to exclude
#' @return This code extracts the src code, and the header, or returns ""
#' @export
#' @author Mark Cowley, 2009-10-13
#'
import.function.header <- function(func, src.root=getOption("src.root"), src.files=NULL, strip.comment.char=FALSE, exclude.patterns=c("~", ".Rcheck", ".git", ".svn")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))

	f <- find.src.file(func, src.root, src.files, exclude.patterns=exclude.patterns)
	if( length(f) == 0 || (length(f)==1 && is.na(f))) {
		stop("No src code containing func was found.\n")
	} else if( length(f) > 1 ) {
		warning("Found multiple files that match this function signature\n")
		f <- f[1]
	}

	import.function.header.from.src.file(func, f, strip.comment.char)
}


#' Import the function header from within a source file.
#'
#' I usually write detailed comment headers before functions in source code files.
#' scenarios:\cr
#'	1. function header preceded by comment block preceded by (blank line, or top of file)\cr
#'	2. function header preceded by (blank line or top of file) -> no comment has been written.\cr
#'	
#' @param func a function name, or the function's code
#' @param f The path to the file containing the function.
#' @param strip.comment.char logical: if TRUE, strip the leading comment characters. 
#' @return This code extracts the src code, and the header, or returns ""
#' @export
#' @author Mark Cowley, 2009-10-13
#'
import.function.header.from.src.file <- function(func, f, strip.comment.char=FALSE) {
	!missing(func) || stop("must specify a function")
	(!missing(f) && (length(f) == 1) && !is.na(f) && file.exists(f)) || stop("must specify 1 input file")

	if ( !is.character(func) )
		func <- as.character(substitute(func))
	
	src <- readLines(f, warn=FALSE)

	pattern <- paste("^[[:space:]]*", func, "[[:space:]]*<-[[:space:]]*function", sep="")
	func.start.line <- grep(pattern, src)
	comment.lines <- grep("^#", src)
	blank.lines <- c(0, which(nchar(src) == 0))
	
	comment.header <- ""
	if ( (func.start.line - 1) %in% comment.lines ) {
		# Then there is a comment line preceeding this function header.
		blanks.above <- blank.lines[blank.lines < func.start.line]
		header.start <- blanks.above[length(blanks.above)] + 1
		header.stop <- func.start.line - 1
		# if( ! src[header.start] %in% comment.lines ) {
		# 	warning("I'd expect a comment line to be after a blank line...\n")
		# }
		comment.header <- src[c(header.start:header.stop)]
	}
	
	if( strip.comment.char ) {
		comment.header <- sub("^#+ ", "", comment.header)
		comment.header <- sub("^#+$", "", comment.header)
		comment.header <- sub("^#(\t+)", "\\1", comment.header)
	}
	
	return(comment.header)
}
