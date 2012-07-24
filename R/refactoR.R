#' refactoR R code
#'
#' under development.
#' 
#' @param func function or function name
#' @param source.pkg character(1) of the source package name (ie where the function is currently defined)
#' @param dest.pkg character(1) of the destination package name (ie where you want the function to be defined)
#' @param src.root the root directory containing your codebase.
#' 
#' @return undefined.
#' 
#' @author Mark Cowley, 2012-07-20
#' @export
#' @importFrom mjcbase tocsv file.move
refactoR <- function(func, source.pkg, dest.pkg, src.root=getOption("src.root", "~/src/R")) {
	r.file <- find.src.file(func, src.root)
	length(r.file) == 1 || warning(sprintf("%s is found in >1 files: %s", as.character(func), tocsv(r.file)))
	r.file <- r.file[1]
	
	r.funcs <- functions.in.source.file(r.file)
	if( length(r.funcs) > 1 ) warning(sprintf("While looking for %s within %s, found %d functions", as.character(func), r.file, length(r.funcs)))
	
	dest.pkg.path <- file.path(src.root, dest.pkg, "R")
	file.exists(dest.pkg.path) || stop(paste("Can't write to", dest.pkg.path))
	file.move(func, dest.pkg.path)
	
	#
	# search for @@imports source.pkg func statements
	#
	stale.imports <- search.function.importsMe(func, source.pkg, src.root)
	if( length(stale.imports) > 0 ) {
		replace.function.importFrom(func, source.pkg, dest.pkg, file)
	}
}

#' find the functions, which @@importFrom 'func'
#' 
#' search within a code base defined by \code{src.root}, for functions
#' which contain @@importFrom 'pkg' 'func'
#'
#' @inheritParams refactoR
#' @param pkg the name of a package which contains \code{func}
#' 
#' @return a \code{list} of elements, 1 per file in which the @@importFrom
#'  directive is found. each element contains:\cr
#' \item{package}{the package name containing the import directive}
#' \item{srcref}{The path to the file containing the import directive}
#' \item{funcs}{a charcter vector of function name(s) which contain the import directive}
#' If the element is not found, an empty \code{list} is returned.
#' 
#' @author Mark Cowley, 2012-07-16
#' @export
#' @examples
#' search.function.importsMe("rename.column", "pwbc")
search.function.importsMe <- function(func, pkg, src.root=getOption("src.root", "~/src/R")) {
	pattern <- sprintf("@import.*\\b%s\\b.*\\b%s\\b.*", pkg, as.character(func))
	cmd <- sprintf("ack -l '%s' %s", pattern, shQuote(normalizePath(src.root)))
	# cat(cmd)
	suppressWarnings(file.matches <- system(cmd, intern=TRUE, ignore.stderr=TRUE))
	attr(file.matches, "status") <- NULL
	# cat(paste(imports.statements, collapse="\n"))

	.getfunc.match.header <- function(file, pattern) {
		code <- readLines(file, warn=FALSE)
		matches <- grep(pattern, code)
		funcs <- grep("<- +function", code)
		res <- numeric(0)
		for(match in matches) {
			func <- funcs[min(which(funcs > match))]
			res <- c(res, func)
		}
		res <- unique(res)
		res <- sub(" .*", "", code[res])
		res
	}
	# debug(.getfunc.match.header)
	# imports.statements
	if( length(file.matches) > 0 ) {
		# pkg.matches <- basename(dirname(dirname(file.matches)))
		
		res <- list()
		for(i in seq(along=file.matches)) {
			# eg file.matches[i] # "/Users/marcow/src/R/ICGCPancreas/R/build.exp_array.log.R"
			res[[i]] <- list()
			res[[i]]$package <- basename(dirname(dirname(file.matches[i]))) # "ICGCPancreas"
			res[[i]]$srcref <- file.matches[i]
			res[[i]]$funcs <- .getfunc.match.header(file.matches[i], pattern)
		}
	}
	else {
		res <- list()
	}
	
	res
}

# search.function.usesMe <- function(func, pkg, src.root=getOption("src.root", "~/src/R")) {
# 	func <- as.character(func)
# 	pkgs <- list.my.packages(src.root=src.root)
# 	pkgs.ns.files <- file.path(src.root, pkgs, "NAMESPACE")
# 	ns.imports <- lapply(pkgs.ns.files, roxygen2:::namespace.file.imports)
# 	pkgs.importMe <- sapply(ns.imports, function(x) pkg %in% x)
# 	pkgs <- pkgs[pkgs.importMe]
# 	
# 	if( length(pkgs) > 0 ) {
# 		
# 	}
# }


#' replace the @@importFrom statements with a new package
#'
#' Given a \code{file} which contains an \code{@@importFrom source.pkg func}
#' directive, replace this with \code{@@importFrom dest.pkg func}
#' 
#' @inheritParams refactoR
#' 
#' @return nothing it will edit the src file, so be very careful!
#' 
#' @author Mark Cowley, 2012-07-16
#' @export
#' @importFrom roxygen2 parse.files
replace.importFrom <- function(func, source.pkg, dest.pkg, file) {
	hdr <- import.function.header.from.src.file(func, file)
	
	pattern <- sprintf("@importFrom.*\\b%s\\b.*\\b%s\\b.*", pkg, as.character(func))
	parsed <- parse.files(file)
}


edit.roxygenheader <- function(hdr) { hdr <- NULL }
