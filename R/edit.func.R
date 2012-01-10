#' edit an R source file
#' 
#' Search for the .R file that contains a function, and open it in an editor.
#' It searches for the file containing the function definition, by grepping
#' for the pattern \code{"^my.func.name <- function"}, where \code{my.func.name}
#' is the name of func.
#'
#' There are functions which work with textmate on OSX (textmate, tm, mate; 
#' smultron), or on Linux (kate = KDE Advanced Text Editor).
#' 
#' Common usage scenarios are that you have a folder full of R packages under
#' development. In this case, set \code{src.root}, or better yet, the
#' \code{options(src.root="/path/to/source/root")} to point to the top level
#' folder containing all the R packages. 
#' If you also have a directory full of .R files, then you can provide these
#' files via the \code{src.files} argument.
#' 
#' @param func the quoted function name, or the function's code
#' @param src.root The parent folder of source code
#' @param src.files an optional vector of filenames to search within
#' @param editor the path to your editor. Default = \code{getOption("editor")}
#' @param exclude.patterns vector of patterns passed to grep for files to exclude.
#' @return nothing. The side effect is that an R source file is opened in an editor.
#' @author Mark Cowley, 26/2/07
#' @export
#' @rdname edit.src.file
#' @seealso \code{\link{find.src.file}}
#' @examples
#' \dontrun{
#' edit.src.file("edit.src.file", "~/src/R")
#' edit.src.file("edit.src.file", "~/src/R", editor="open -a TextMate")
#' }
edit.src.file <- function(func, src.root=getOption("src.root"), src.files=NULL, editor=getOption("editor"), exclude.patterns=c("~", ".Rcheck", ".git", ".svn")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	if( is.null(editor) )
		editor <- getOption("editor")

	f <- find.src.file(func, src.root, src.files, exclude.patterns=exclude.patterns)
	if( length(f) > 0 ) {
		pattern <- paste("^", func, " <- function", sep="")
		line <- system( paste("grep -n", shQuote(pattern), shQuote(get.full.path(f))), intern=T )
		lineno <- as.numeric( strsplit(line, ":")[[1]][1] )
		system( paste(editor, shQuote(f), " >/dev/null 2>/dev/null &") )

		p <- which.package(func, verbose=FALSE)
		# cat(paste("goto line: ",lineno,", then call:\tupdate.", p,"()\n", sep=""))
		cat(sprintf("goto line: %d, then call:  updateR(\"%s\")\n", lineno, p))
	}
}
# CHANGELOG
# 2011-11-24:
# - major roxygen merge of all functions in this file.
# - bug fix each wrapper function to substitute the value of func before
#   calling edit.src.file
# 2012-01-10
# - wrapped package name in quotes in the final cat(sprintf( command
# 


#' @rdname edit.src.file
#' @export
kate <- function(func, src.root=getOption("src.root")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	edit.src.file(func=func, src.root=src.root, editor="/usr/bin/kate -u")
}

#' @rdname edit.src.file
#' @export
textmate <- function(func, src.root=getOption("src.root")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	edit.src.file(func=func, src.root=src.root, editor="open -a TextMate")
}

#' @rdname edit.src.file
#' @export
tm <- function(func, src.root=getOption("src.root")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	textmate(func, src.root)
}

#' @rdname edit.src.file
#' @export
mate <- function(func, src.root=getOption("src.root")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	textmate(func, src.root)
}

#' @rdname edit.src.file
#' @export
smultron <- function(func, src.root=getOption("src.root")) {
	if ( !is.character(func) )
		func <- as.character(substitute(func))
	edit.src.file(func=func, src.root=src.root, editor="open -a Smultron")
}
