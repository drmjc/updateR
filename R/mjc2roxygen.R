#' Convert all the comment headers into roxygen headers
#'
#' This function takes a package root directory, parses all its R
#' files under the R directory and update the corresponding R
#' source code by inserting roxygen documentation in to the R
#' scripts.
#' 
#' This is my version of \code{\link[Rd2roxygen]{Rd2roxygen}}, and 90\% of it is
#' from Rd2roxygen. It parses all functions, within all source files in a package,
#' and inserts roxygenized versions of those same comments.
#'
#' @param pkg the root directory of the package
#' @param nomatch PROBABLY HAS NO EFFECT IN MY CODE
#'          the file name (base name only) to use when an object in the
#'          Rd file is not found in any R source files (typically this
#'          happens to the data documentation); if not specified, the
#'          default will be `pkg'-package.R
#' @param usage logical: whether to include the usage section in the output
#'    \code{roxygen} can auto-generate these.
#' @value NULL (but the process of conversion will be printed on screen)
#' @note set verbatim{options(roxygen.comment = "##' ")} to signify that these
#'   roxygen comments are auto-generated, and use \verbatim{"#' "} for 
#'   manually generated comments.
#' @examples
#'  # not run
#'  # mjc2roxygen("~/src/R/mjcdev")
#' @export
mjc2roxygen <- function (pkg, nomatch, usage = FALSE) {
	require(updateR)
	require(Rd2roxygen)
	if (!all(c("R") %in% list.files(pkg))) 
		stop("'pkg' has to be the root directory of a source package")

	roxComment <- getOption("roxygen.comment"); if(is.null(roxComment)) roxComment <- "#' "
	
	R.dir <- file.path(pkg, "R")
	files <- list.files(R.dir, "\\.[Rr]$")
	if (missing(nomatch)) 
		nomatch <- paste(basename(pkg), "-package.R", sep = "")
	unlink(p <- file.path(R.dir, nomatch))
	for (f in files) {
		src <- readLines(file.path(R.dir, f), warn=FALSE)
		src <- grep("^[a-zA-Z].* <- function", src, value=TRUE)
		if( length(src) == 0 ) next
		functions <- trim(sub(" *<-.*", "", src))
		for(func in functions) {
			timestamp()
			cat(func, "\t", file.path(R.dir, f), "\n")
			hdr <- import.function.header.from.src.file(func, file.path(R.dir, f))
			if( length(hdr) == 1 && hdr == "" ) next
			# else cat(hdr)
			parsed <- mjcheader2roxygen(hdr)
			if( is.character(parsed) && length(parsed) == 1 && parsed == "" ) next
			parsed$name <- func
			parsed$desc <- ifelse(parsed$desc=="", func, parsed$desc)
			Rd <- create_roxygen(parsed, usage = usage)
			# delete empty lines without even a roxygen comment
			Rd <- Rd[Rd != "\n" & Rd != ""]
			# condense consecutive lines of just roxygen comments (this happens when details is empty)
			if( any(Rd==roxComment) ) {
				for(i in rev(which(Rd==roxComment))) {
					if(i>0 && Rd[i-1] == roxComment) Rd <- Rd[-i]
				}
			}
			# remove empty @examples\n"" sections
			if( any(grepl("@examples", Rd)) ) {
				idx <- grep("@examples", Rd)
				if( (idx + 1) == length(Rd) && Rd[idx+1] == roxComment ) 
					Rd <- Rd[-c(idx, idx+1)]
			}
		 	Rd <- c(Rd, sprintf("%s@export", roxComment))
			message("parsed: ", f)
			fname <- parsed$name
			tryf <- f
			idx <- integer(0)
			message("looking for the object '", fname, "' in:")
			for (i in tryf) {
				r <- file.path(R.dir, i)
				idx <- grep(sprintf("^[[:space:]]*(`|)(%s)(`|)[[:space:]]*(<-|=)[[:space:]]*function", gsub("\\.", "\\\\.", fname)), 
						(r.Rd <- readLines(r, warn = FALSE)))
				message("  ", i, ": ", appendLF = FALSE)
				message(ifelse(length(idx), paste("row", idx), "not found"))
				if (length(idx)) 
					break
			}
			if (length(idx)) {
				cat(append(r.Rd, c("\n", Rd), idx - 1), file = r, 
					sep = "\n")
				message(r, " updated")
			}
			else {
				cat(c("\n", Rd, "NULL"), "\n\n", file = p, sep = "\n", 
					append = TRUE)
				message("unmatched object '", fname, "' written into ", 
					p)
			}
			message("\n")
			if (.Platform$OS.type == "windows") {
				flush.console()
			}
		}
	}
}

#' Trim whitespace
#'
#' Trim the whitespace from front and back of the words in a vector.
#'
#' @param x [character] a character vector
#' @return character vector of trimmed text
#' @author Mark Cowley, 2009-08-19
#' @export
#'
trim <- function(x) {
	x <- sub("^[ \t]+", "", x)
	x <- sub("[ \t]+$", "", x)
	x
}


#' Parse a function header comment into roxygen list
#' @param hdr a character vector containing the parsed header.
#' @param docType default docType for the function
#' @param format default format for the function
#' @param keywords default keywords for the function
#' @param default.author the default author. This is ignored if author is set
#' @value 
#' @seealso \code{\link[Rd2roxygen]{parse_file}}, \code{\link{import.function.header}}, \code{\link{import.function.header.from.src.file}}
#' @examples
#' hdr <- import.function.header.from.src.file("mjcheader2roxygen", "~/src/R/updateR/R/mjc2roxygen.R")
#' rox <- mjcheader2roxygen(hdr)
#' str(rox)
#' @export
mjcheader2roxygen <- function(hdr, docType="", format="", keywords="", default.author="Mark Cowley") {
	# is the header already roxygenized?
	roxComment <- getOption("roxygen.comment", default="#' ")
	
	if( any(grepl("^#+'", hdr)) ) return("")
	pvd <- "Parameters:" %in% hdr && "Value:" %in% hdr && "Details:" %in% hdr
	
	hdr <- sub("^#+", "", hdr)
	hdr <- trim(hdr)
	# Trim empty lines from start and end of the header.
	while(TRUE) {
		if( hdr[1] == "" ) hdr <- hdr[2:length(hdr)]
		else break
	}
	while(TRUE) {
		if( hdr[length(hdr)] == "" ) hdr <- hdr[1:(length(hdr)-1)]
		else break
	}
	res <- list()
	if( length(hdr) > 0 ) {
		# cat(hdr, "\n")
		res$desc <- .escape.specials(.parse.description(hdr))
		res$params <- .escape.specials(.parse.parameters(hdr))
		res$details <- .escape.specials(.parse.details(hdr))
		res$examples <- .escape.specials(.parse.examples(hdr))
		res$usage <- ""
		res$author <- .escape.specials(.parse.author(hdr))
		if( length(res$author) == 1 && res$author == "" ) res$author <- default.author
		res$value <- .escape.specials(.parse.value(hdr))
		res$docType <- docType
		res$format <- format
		res$note <- .escape.specials(.parse.notes(hdr))
		res$aliases <- NULL
		res$keywords <- keywords
		# res <- lapply(res, function(x) ifelse(length(x)==0, "", x))
	}
	
	res
}

.escape.specials <- function(x) {
	if( is.list(x) && length(x) > 0 ) 
		x <- lapply(x, .escape.specials)
	else if( length(x) > 0 ) {
		x <- gsub("@", "@@", x)
		x <- gsub("%", "\\%", x)
		# avoid double escaping those % signs.
		x <- gsub("\\\\%", "\\%", x)
		x <- gsub("\\t", "\\\\t", x)
		x <- gsub("\\n", "\\\\n", x)
	}
	x
}

.parse.description <- function(hdr) {
	if( length(hdr) == 0 || (length(hdr) == 1 && hdr == "") ) return("")
	# keep from top until a delimiter is hit.
	delimiter.lines <- c(
		which(hdr %in% c("", "Value:", "Details:", "Examples:")),
		grep("Mark Cowley", hdr)
	)
	if( length(delimiter.lines) > 0 ) {
		hdr <- hdr[-c(min(delimiter.lines):length(hdr))]
	}
	if( length(hdr) == 0 || (length(hdr) == 1 && hdr == "") ) 
		return ("")
	# if first header line ends in full stop, then that's the description
	# else, collapse all header lines into 1 sentence.
	if(substring(hdr[1], nchar(hdr[1]), nchar(hdr[1])) == ".")
		return (hdr[1])
	else 
		return( paste(hdr, collapse=" ") )
}

.parse.parameters <- function(hdr) {
	res <- c()
	if("Parameters:" %in% hdr) {
		idx <- which(hdr=="Parameters:")
		length(idx)==1 || stop("found 2 parameters sections.")
		hdr <- hdr[(idx+1):length(hdr)]
		delimiter.lines <- which(hdr %in% c("", "Parameters:", "Details:", "Examples:", "Notes:", "Value:"))
		delimiter.lines <- c(delimiter.lines, grep("Mark Cowley", hdr), grep("^see also", hdr, ignore=TRUE))
		hdr <- hdr[1:(min(delimiter.lines)-1)]
		parts <- strsplit(hdr, ":")
		for(i in 1:length(parts)) {
			if( length(parts[[i]])==2 ) {
				param <- parts[[i]][1]
				descr <- parts[[i]][2]
				param <- trim(param)
				descr <- trim(descr)
				if( grepl(",", param) ) {
					params <- strsplit(param, ",")[[1]]
					params <-  trim(params)
					for(param in params) {
						res <- c(res, paste(param, descr, sep=" "))
						# res[[param]] <- descr
					}
				}
				else {
					res <- c(res, paste(param, descr, sep=" "))
					# res[[param]] <- descr
				}
			}
			else if( length(parts[[i]])==1 ) {
				# continuation comment line.
				descr <- parts[[i]][1]
				descr <- trim(descr)
				# res[[length(res)]] <- paste(res[[length(res)]], descr, collapse=" ")
				res[length(res)] <- paste(res[length(res)], descr, sep=" ")
			}
			else if( length(parts[[i]]) > 2 ) {
				# description line which contains a colon... assume 1st colon = param, other colons are just punctuation
				param <- parts[[i]][1]; param <- trim(param)
				descr <- parts[[i]][2:length(parts[[i]])]
				descr <- paste(descr, collapse=":")
				descr <- trim(descr)
				# @TODO: detect :: and replace with \code{\link[package]{function}}
				# if( grepl("::", descr) ) {
				#	
				# }
				# res[[param]] <- descr
				res <- c(res, paste(param, descr, sep=" "))
			}
		}
		
		if( any(grepl("^\\.\\.\\.", res)) ) {
			res <- sub("^\\.\\.\\.", "\\dots", res)
		}
	}
	
	return(res)
}

.parse.details <- function(hdr) {
	res <- list()
	if("Details:" %in% hdr) {
		idx <- which(hdr=="Details:")
		length(idx)==1 || stop("found 2 Details sections.")
		hdr <- hdr[(idx+1):length(hdr)]
		while( length(hdr) > 0 ) {
			if( hdr[1] %in% c("Parameters:", "Details:", "Examples:", "Notes:", "Value:") ) break
			else if( grepl("Mark Cowley", hdr[1]) ) break
			else {
				res[[length(res)+1]] <- hdr[1]
			}
			# pop the top line off the hdr
			hdr <- hdr[-1]
		}
	}
	else {
		# if the first line ends in fullstop, then 2nd line onwards = Details.
		if( nchar(hdr[1]) > 0 && length(hdr) > 1 &&
			substring(hdr[1], nchar(hdr[1]), nchar(hdr[1])) == "." &&
			hdr[2] != "" ) {
			top <- 2
		}
		else {
			top <- which(hdr == "")+1
		}
		bottom <- c(which(hdr == "Value:" | hdr == "Parameters:" | hdr == "Examples:" | hdr == "Notes:"), 
					grep("Mark Cowley", hdr), length(hdr))
		if( length(top>0) && length(bottom>0) && top[1] < bottom[1]-1 ) {
			res <- hdr[(top[1]):(bottom[1]-1)]
			while(TRUE) {
				if(res[length(res)]=="") res <- res[1:(length(res)-1)]
				else break
			}
		}
	}
	if( length(res) == 0)  return("")
	else return(paste(unlist(res), sep=" "))
	# return(res)
}


.parse.value <- function(hdr) {
	res <- list()
	if("Value:" %in% hdr) {
		idx <- which(hdr=="Value:")
		length(idx)==1 || stop("found 2 Value sections.")
		hdr <- hdr[(idx+1):length(hdr)]
		while( length(hdr) > 0 ) {
			if( hdr[1] %in% c("Parameters:", "Details:", "Examples:", "Notes:", "Value:") ) break
			else if( grepl("Mark Cowley", hdr[1]) ) break
			else {
				res[[length(res)+1]] <- hdr[1]
			}
			# pop the top line off the hdr
			hdr <- hdr[-1]
		}
		res <- paste(res, collapse=" ")
		res <- trim(res)
	}
	else {
		res <- ""
	}

	return(res)
}

.parse.notes <- function(hdr) {
	tag <- "Notes:"
	res <- list()
	if(tag %in% hdr) {
		idx <- which(hdr==tag)
		length(idx)==1 || stop(sprintf("found 2 %s sections.", tag))
		hdr <- hdr[(idx+1):length(hdr)]
		while( length(hdr) > 0 ) {
			if( hdr[1] %in% c("Parameters:", "Details:", "Examples:", "Notes:", "Value:") ) break
			else if( grepl("Mark Cowley", hdr[1]) ) break
			else {
				res[[length(res)+1]] <- hdr[1]
			}
			# pop the top line off the hdr
			hdr <- hdr[-1]
		}
		res <- paste(res, collapse=" ")
	}
	else {
		res <- ""
	}

	return(res)
}


.parse.examples <- function(hdr) {
	tag <- "Examples:"
	res <- list()
	if(tag %in% hdr) {
		idx <- which(hdr==tag)
		length(idx)==1 || stop(sprintf("found 2 %s sections.", tag))
		hdr <- hdr[(idx+1):length(hdr)]
		while( length(hdr) > 0 ) {
			if( hdr[1] %in% c("Parameters:", "Details:", "Examples:", "Notes:", "Value:") ) break
			else if( grepl("Mark Cowley", hdr[1]) ) break
			else {
				res[[length(res)+1]] <- hdr[1]
			}
			# pop the top line off the hdr
			hdr <- hdr[-1]
		}
		res <- res[res!=""]
		res <- unlist(res)
	}
	else {
		res <- ""
	}

	return(res)
}


.parse.author <- function(hdr) {
	res <- ""
	if( any(grepl("Mark Cowley", hdr)) ) {
		res <- grep("Mark Cowley", hdr, value=TRUE)
	}
	else {
		res <- ""
	}
	res
}
