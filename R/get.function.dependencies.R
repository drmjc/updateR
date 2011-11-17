#' Determine all the dependencies of a function from a list of packages.
#'
#' For a given function, exhaustively determine which functions are in turn
#' called by that function. This is designed to search within a subset of
#' packages - perhaps ones that you are actively coding. This method will also
#' tell you which CRAN or BioConductor packages are also referenced.
#' 
#' If you've ever wanted to give someone a single function from within a
#' package full of code, then you've probably forgotten about some of the code
#' that this function depends upon, which you wrote & which is not available on
#' CRAN/BioC. Given a single function, this method will determine all the code
#' dependencies, ie all the functions which will ever get called. This is
#' exhaustive, in that it doesn't only look for functions inside conditional
#' statements -- all functions are returned. It only searches within a limited
#' set of packages, such as those that you may have written yourself & not
#' published yet. It will also tell you which CRAN/BioC packages need to be
#' installed, but will ignore those packages which are automatically loaded at
#' startup.
#' 
#' @param fun either a function name, of the function's code
#' @param packages vector of packages to search for dependencies in (ie
#'   packages that you probably wrote), Include \code{.GlobalEnv} if you want
#'   to trace inside functions which may be loaded in the workspace.
#' @param verbose print the recursive dependency trace
#' @return A list with these elements: 
#'   \item{my.functions}{a character vector of all functions that are called, 
#'       that are also in the \code{my.packages} packages}
#'   \item{my.function.packages}{a vector indicating which package each function came from} 
#'   \item{cran}{a vector of CRAN packages that are referenced} 
#'   \item{bioc}{a vector of BioConductor packages that are referenced} 
#'   \item{other.packages:}{a vector of other packages not from CRAN, or 
#'       BioConductor, or my.packages.}
#' @author Mark Cowley
#' @seealso \code{\link[codetools]{findGlobals}}, \code{\link{list.my.packages}}
#' @keywords utilities
#' @export
#' @examples
#' options(src.root="~/src/R")
#' get.function.dependencies("updateR")
#' get.function.dependencies(updateR)
get.function.dependencies <- function( fun, 
							  packages=c(list.my.packages(src.root=getOption("src.root")), ".GlobalEnv"), 
							  verbose=FALSE ) {
	if ( !is.character(fun) ) fun <- as.character(substitute(fun))
	require(codetools)
	known.packages <<- NULL
	depth <<- 0
	res <- .get.function.dependencies(fun, packages=packages, verbose=verbose)
	rm(known.packages, pos=1)
	rm(depth, pos=1)
	
	# i'm not sure why some functions can be duplicated
	if( any(duplicated(res$my.functions)) ) {
		ufunc <- unique(res$my.functions)
		o <- match(ufunc, res$my.functions)
		res$my.functions <- res$my.functions[o]
		res$my.function.packages <- res$my.function.packages[o]
	}
	
	# reorder results
	o <- order(res$my.function.packages, res$my.functions)
	res$my.functions <- res$my.functions[o]
	res$my.function.packages <- res$my.function.packages[o]

	# annotate other.packages by cran or bioc status.
	res$cran <- intersect(res$other.packages, available.packages.cran()[,"Package"])
	res$bioc <- intersect(res$other.packages, available.packages.bioc()[,"Package"])
	res$cran <- setdiff(res$cran, res$bioc) # some bioc packages are also on cran (eg limma)
	res$other.packages <- setdiff(res$other.packages, c(res$cran, res$bioc))
	
	res
}
# CHANGELOG
# 2011-04-07: version 1
# 2011-04-19: somehow some duplicates sneak through the .get.function.dependencies call.


#' Determine all the dependencies of a function from a list of packages and src files and export them into a file
#'
#' If you've ever wanted to give someone a single function from within a package full of code, then you've probably
#'  forgotten a few other code functions & gone back & forth.
#' Given a single function, this method will determine all the code dependencies. 
#' It only searches within a limited set of packages, such as those that you may have written yourself & not published
#'  yet.
#' It will also tell you which CRAN/BioC packages need to be installed, but will ignore those packages which are
#'  automatically loaded at startup.
#'
#' @param fun either a function name, of the function's code
#' @param file path to a file to write to
#' @param packages vector of packages to search for dependencies in (ie packages that you probably wrote)
#' @param src.files optional vector of R sources files to also search within. These have higher priority than code
#'	within packages.
#' @param verbose logical: verbose output?
#' @return nothing. writes a file containing source code.
#' @author Mark Cowley, 2011-04-07
#' @export
#' @examples
#' \dontrun{
#' f <- tempfile()
#' get.function.dependencies2file(fun=edit.src.file, file=f)
#' get.function.dependencies2file(fun=edit.src.file, file=f, src.files=NULL, verbose=TRUE)
#' unlink(f)
#' }
#'
get.function.dependencies2file <- function(	fun,
											file, 
											packages=c(list.my.packages(src.root=getOption("src.root")), ".GlobalEnv"), 
											src.files=NULL,
											verbose=FALSE) {

	!missing(fun) || stop("Must specify a function, either by quoted or unquoted name.")
	!missing(file) || stop("Must specify file")
	unlink(file)

	if ( !is.character(fun) ) fun <- as.character(substitute(fun))
	
	deps <- get.function.dependencies(fun, packages=packages, verbose=verbose)

	all.funcs <- c(fun, deps$my.functions)

	f <- file(file, "w")
	
	#
	# for all the functions, either within packages, or .GlobalEnv:
	#
	funcs <- all.funcs
	# reorder functions so those from the same file are grouped together (aesthetics)
	files <- sapply(funcs, find.src.file, src.root=getOption("src.root"), src.files=src.files, unique=TRUE)
	o <- order(files, na.last=FALSE)
	funcs <- funcs[o]
	files <- files[o]
	
	# src code either exists in one of the src.files; within src.root; within .GlobalEnv.
	# if it's only in .GlobalEnv, then the best we can do is print the definition that
	# we can retrieve using get(). This will not contain any in line comments though, as
	# these get stripped out during source().
	for(i in which(is.na(files))) {
		# we'd better hope there's a function definition for these within .GlobalEnv, or
		# we are out of luck			
		func <- funcs[i]
		if( exists(func, where=1) ) {
			writeLines(sprintf("###\n### from %s\n###\n", ".GlobalEnv"), f)
			func.code <- get(func, pos=1)
			func.code <- deparse(func.code)
			func.code[1] <- paste(func, "<-", func.code[1], collapse=" ")
			func.code <- gsub("    ", "\t", func.code)
			func.code <- c(func.code, "")
			writeLines(func.code, f)
		}
		else {
			warning(sprintf("Couldn't find the src code for '%s' in src.root, src.files, or .GlobalEnv", func))
		}
	}
	for(i in which(!is.na(files))) {
		func <- funcs[i]
		writeLines(sprintf("###\n### from %s\n###\n", files[i]), f)
		func.code <- import.function(func, src.root=getOption("src.root"), src.files=src.files)
		func.code <- c(func.code, "")
		writeLines(func.code, f)
	}
	
	close(f)
	
}


#' Recursively determine the dependencies of a given function.
#'
#' Not intended for public consumption. See \code{\link{get.function.dependencies}}
#'
#' @param fun either a function name, of the function's code
#' @param packages vector of packages to search for dependencies in (ie packages that you probably wrote)
#' @param ignore.packages a vector of packages to ignore. defaults to the list of packages loaded at startup.
#' @param verbose print the recursive dependency trace 
#' @return a list with these elements:
#'	\item{my.functions}{a vector of all functions that are called}
#'	\item{my.function.packages}{a vector indicating which package each function came from}
#'	\item{other.packages}{a vector of other CRAN or BioConductor packages which are also needed}
#' @author Mark Cowley
#' @noRd
.get.function.dependencies <- function(fun, packages, ignore.packages=c("base", getOption("defaultPackages")), src.root=getOption("src.root"), verbose=FALSE) {
	require(codetools)
	if ( !is.character(fun) ) fun <- as.character(substitute(fun))
	depth <<- depth + 1
	prefix <- paste(rep(">", depth), collapse="")
	if( verbose ) cat(prefix, "Parsing:", fun, "\n")
	
	# find all the functions that are called by 'fun' & the packages they're from
	fun.code <- try(get(fun), silent=TRUE)
	if( is(fun.code, "try-error") ) {
		res <- list(my.functions=c(), my.function.packages=c(), other.packages=c())
		return(res)
	}
	else {
		functions <- findGlobals(fun.code, merge=FALSE)$functions
		function.packages <- which.package(functions, verbose=FALSE, unique=TRUE, src.root=src.root)
		length(functions) == length(function.packages) || stop("length(functions) != length(function.packages)")
	}
	
	# print(cbind(functions, function.packages))
	
	#
	# filter out the functions from the default packages, such as base, grDevices, ...
	#
	i <- which(function.packages %in% ignore.packages )
	if( length(i) > 0 ) {
		functions <- functions[-i]
		function.packages <- function.packages[-i]
	}
	
	# if which.package couldn't find the package, then there's 2 common reasons:
	# 1) missing a package which is not loaded in the current session
	# 2) the code references a hidden function that is not exported from a namespace. 
	# The former probably means you need a call to library() or require()
	# The latter is hopefully addressed by which.package,
	if( any(is.na(function.packages)) ) {
		warning("Can't determine the package for these functions:\n", paste(functions[is.na(function.packages)], collapse="\n"), "\n")
	}

	#
	# remember which packages are not mine, & then filter them out
	#
	other.packages <- NULL
	i <- which(! function.packages %in% packages )
	if( length(i) > 0 ) {
		other.packages <- unique(function.packages[i])
		functions <- functions[-i]
		function.packages <- function.packages[-i]
	}
	
	# all remaining functions should be from packages
	my.functions <- functions
	my.function.packages <- function.packages
	if( verbose && length(my.functions)>0 ) cat(prefix, "Found:", my.functions, "\n")
	
	# # hidden functions, eg '.gsea.get.classes.index.html' which are not exported from a namespace
	# # are very tricky. you can't use ls(..., all.names=TRUE), find(), objects() or get() on them.
	# # You can only see them by typing "metaGSEA:::.gsea.get.classes.index.html" (without the quotes)
	# i <- grep("^\\.", my.functions)
	# if( length(i)>0 ) {
	# 	warning("Can't locate, nor auto parse these functions: ", paste(my.functions[i], collapse=", "))
	# 	my.functions <- my.functions[-i]
	# 	my.function.packages <- my.function.packages[-i]
	# }
	
	# recurse through the functions that may be scattered throughout your codebase
	if( length(my.functions) > 0) {
		for(my.fun in my.functions) {
			if( ! my.fun %in% known.packages ) {
				if( verbose ) cat(prefix, "recursing into:", dQuote(my.fun), "\n")
				known.packages <<- c(known.packages, my.fun)
				tmp <- .get.function.dependencies(my.fun, packages=packages, ignore.packages=ignore.packages, verbose=verbose)
				my.functions         <- c(my.functions        , tmp$my.functions)
				my.function.packages <- c(my.function.packages, tmp$my.function.packages)
				other.packages       <- union(other.packages,   tmp$other.packages)
			}
		}
	}
	depth <<- depth - 1
	
	res <- list(my.functions=my.functions, my.function.packages=my.function.packages, other.packages=other.packages)
	res
}
# CHANGELOG:
# 2011-04-07: version 1
