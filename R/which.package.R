#' Function to tell you which package a function came from.
#' 
#' Given a function, identify which package(s) it was defined in. Most packages
#' now have namespaces, so functions can either be exported, or not.
#' If your function is exported in a namespace, then no problems.
#' If your function is not exported (usually hidden functions starting with \sQuote{.},
#' eg: \code{.gsea.get.classes.index.html} from \code{metaGSEA},
#' then there's no facility in native \R to determine this (AFAICT).
#' By searching through a collection of source code, you may find the file
#' that contains the function, and thus which package its from.
#' @note 
#' If your function is \emph{hidden} (ie, starts with a dot), then you must use 
#' its quoted name when passing to \code{func=".my.hidden.function"},
#' otherwise doing this: \code{func=.my.hidden.function}, will cause the \R
#' interpreter to complain that it can't find \code{.my.hidden.function}.
#'
#' @param func either the quoted function name (eg \dQuote{plot}), or the function's
#'   name (eg \code{plot}). Can be a character vector of multiple function's names.
#' @param unique logical: if \code{TRUE}, only return the first result (some functions are
#'   defined in multiple packages), if \code{FALSE} and > 1 instances are found, then all
#' will be returned
#' @param src.root the root folder containing all your R packages under
#'   development.
#' @param verbose logical
#' @return a vector of package names, 1 per function in \code{func}
#' @author Mark Cowley
#' @examples
#' \dontrun{
#' which.package(plot)
#' }
#' @export
which.package <- function(func, unique=FALSE, src.root=getOption("src.root"), verbose=FALSE) {

	# recurse if necessary.
	if( length(func) > 1 ) {
		res <- NULL
		for(i in 1:length(func)) {
			tmp <- which.package(func[i], unique=unique, src.root=src.root, verbose=verbose)
			tmp <- ifelse(is.null(tmp), NA, tmp)
			res <- c(res, tmp)
		}
		return( res )
	}
	else {
		if( !is.character(func) )
			func <- as.character( substitute(func) )

		#
		# first try to find out which package, by hunting through the list of loaded packages
		#
		loaded.packages <- search()
		# iterate through the packages
		package.name <- package.name.path <- NULL
		for(i in 1:length(loaded.packages)) {
			if( func %in% ls(pos=loaded.packages[i], all.names=TRUE) ) {
				package.name <- c(package.name, loaded.packages[i])
				package.name.path <- c(package.name.path, searchpaths()[i])
			}
			# if unique, then stop searching as soon as we have a valid hit.
			if( unique && !is.null(package.name) ) break
		}
		
		if(!is.null(package.name)) {
			package.name <- sub("package:", "", package.name)
			package.name[package.name == "package:.GlobalEnv"] <- ".GlobalEnv"
		}
		else {
			#
			# if the package hasn't been determined yet, then search through the src files 
			# within src.root to see if the function has been defined there.
			#
			func.file <- find.src.file(func, src.root=src.root)
			if( !is.na(func.file) ) {
				# here's a worked example:
				# func.file = "/Users/marcow/src/R/metaGSEA/R/import.gsea.topTable.R"
				# dirname(dirname(func.file)) = "/Users/marcow/src/R/metaGSEA"
				# basename(dirname(dirname(func.file))) = "metaGSEA"
				package.name <- basename(dirname(dirname(func.file)))
				# todo, order these code according to the order specified by search()
				package.name <- package.name[1]
				package.name.path <- searchpaths()[match(package.name, basename(searchpaths()))]
			}
		}
		
		if( verbose ) {
			if(!is.null(package.name))
				cat(paste(dQuote(func), "found in", dQuote(package.name), "which is installed in:", dQuote(package.name.path)))
		}

		return( package.name )
	}
}
# CHANGELOG
# 2006-11-06: version 1
# 2011-04-08: dropped the support for similar -- apropos should be used to identify similar function names
# 2011-04-08: added src.root & the ability to find functions within code.
# 2011-04-08: changed verbose to FALSE
