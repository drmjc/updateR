#' Search for the .R file that contains a function, and open it in an editor.
#'
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @param src.files an optional vector of filenames to search within
#' @param editor the path to your editor.
#' @param exclude.patterns vector of patterns passed to grep for files to exclude
#' @return nothing. The side effect is that an R source file is opened in an editor.
#' @author Mark Cowley, 26/2/07
#' @export
edit.src.file <- function(func, src.root=getOption("src.root"), src.files=NULL, editor="open -a TextMate", exclude.patterns=c("~", ".Rcheck", ".git", ".svn")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	if( is.null(editor) )
		editor <- options("editor")

	f <- find.src.file(func, src.root, src.files, exclude.patterns=exclude.patterns)
	if( length(f) > 0 ) {
		pattern <- paste("^", func, " <- function", sep="")
		line <- system( paste("grep -n", shQuote(pattern), shQuote(get.full.path(f))), intern=T )
		lineno <- as.numeric( strsplit(line, ":")[[1]][1] )
		system( paste(editor, shQuote(f), " >/dev/null 2>/dev/null &") )

		p <- which.package(func, verbose=FALSE)
		# cat(paste("goto line: ",lineno,", then call:\tupdate.", p,"()\n", sep=""))
		cat(sprintf("goto line: %d, then call:  updateR(%s)\n", lineno, p))
	}
}

#' Edit source file in kate (KDE advanced text editor)
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @return nothing. The side effect is that an R source file is opened in an editor.
#' @author Mark Cowley, 26/2/07
#' @export
kate <- function(func, src.root=getOption("src.root")) {
	edit.src.file(func=func, src.root=src.root, editor="/usr/bin/kate -u")
}

#' Edit source file in TextMate
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @return nothing. The side effect is that an R source file is opened in an editor.
#' @author Mark Cowley, 26/2/07
#' @export
textmate <- function(func, src.root=getOption("src.root")) {
	edit.src.file(func=func, src.root=src.root, editor="open -a TextMate")
}

#' Edit source file in TextMate
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @return nothing. The side effect is that an R source file is opened in an editor.
#' @author Mark Cowley, 26/2/07
#' @export
tm <- function(func, src.root=getOption("src.root")) {
	textmate(func, src.root)
}

#' Edit source file in TextMate
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @return nothing. The side effect is that an R source file is opened in an editor.
#' @author Mark Cowley, 26/2/07
#' @export
mate <- function(func, src.root=getOption("src.root")) {
	textmate(func, src.root)
}

#' Edit source file in Smultron
#' @param func a function name, or the function's code
#' @param src.root The parent folder of source code
#' @return nothing. The side effect is that an R source file is opened in an editor.
#' @author Mark Cowley, 26/2/07
#' @export
smultron <- function(func, src.root=getOption("src.root")) {
	edit.src.file(func=func, src.root=src.root, editor="open -a Smultron")
}
