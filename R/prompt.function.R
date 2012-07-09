#' Produce a prototype of an Rd (R Documentation) file, from a function definition.
#' 
#' Produce a prototype of an Rd (R Documentation) file, from a function definition.
#' In fact, there was never a \code{prompt.function} in \code{\link[utils]{prompt}},
#' just a \code{\link{prompt.default}}, so I haven't over-written anything from \code{utils}.
#' This code originally came from \dQuote{src/library/utils/R/prompt.R}
#' 
#' @param object an R object, typically a function for the default method.
#'           Can be \code{missing} when \code{name} is specified.
#' @param filename usually, a connection or a character string giving the name
#'           of the file to which the documentation shell should be
#'           written.  The default corresponds to a file whose name is
#'           \sQuote{name} followed by \dQuote{.Rd}.  Can also be \sQuote{NA} (see below).
#' @param name a character string specifying the name of the object.
#' @param force.function logical: If \code{TRUE}, treat \sQuote{object} as function in any case.
#' @param \dots further arguments passed to or from other methods.
#' @return
#' If \sQuote{filename} is \code{NA}, a list-style representation of the
#' documentation shell.  Otherwise, the name of the file written to
#' is returned invisibly.
#' @author Mark Cowley, 2009-10-13
#' @export
prompt.function <- function(object, filename = NULL, name = NULL, force.function = FALSE, ...) {
	paste0 <- function(...) paste(..., sep = "")

	is.missing.arg <- function(arg)
		typeof(arg) == "symbol" && deparse(arg) == ""

	if(missing(name))
		name <- if(is.character(object))
			object
		else {
			name <- substitute(object)
			## <FIXME>
			## This used to be:
			##	   if(is.language(name) && !is.name(name))
			##		   name <- eval(name)
			##	   as.character(name)
			## but what is this trying to do?
			## It seems that the eval() will typically give the given
			## object, and surely we cannot use that as the name (even
			## if the subsequent as.character() does not fail ...)
			## Better to be defensive about this, and handle only cases
			## we know will make sense ...
			if(is.name(name))
				as.character(name)
			else if(is.call(name)
					&& (as.character(name[[1]]) %in%
						c("::", ":::", "getAnywhere"))) {
				name <- as.character(name)
				name[length(name)]
			}
			else
				stop("cannot determine a usable name")
			## </FIXME>
		}

	if( is.null(filename) ) {
		filename <- paste0(name, ".Rd")
		# if( !is.null(dir)) {
		# 	filename <- file.path(dir, filename)
		# }
	}
	
	x <- if(!missing(object))
		object
	else {
		## Better than get(); works when called in fun :
		x <- get(name, envir = parent.frame())
	}

	## <FIXME>
	## If not a function or forced to document a function (?), always
	## assume data set.
	if(!(is.function(x) || force.function))
		return(promptData(x, filename = filename, name = name))
	## </FIXME>

	n <- length(argls <- formals(x))
	if(n > 0) {
		arg.names <- arg.n <- names(argls)
		arg.n[arg.n == "..."] <- "\\dots"
	}
	## Construct the 'call' for \usage.
	Call <- paste0(name, "(")
	for(i in seq_len(n)) {						 # i-th argument
		Call <- paste0(Call, arg.names[i],
					   if(!is.missing.arg(argls[[i]]))
					   paste0(" = ",
							  paste(deparse(argls[[i]], width.cutoff= 500),
									collapse="\n")))
		if(i != n) Call <- paste0(Call, ", ")
	}

	Rdtxt <-
		list(name = paste0("\\name{", name, "}"),
			 aliases = paste0("\\alias{", name, "}"),
			 title = "\\title{ Function Title }",
			 description = c("\\description{",
			 paste("  ~~ A concise (1-5 lines) description of what",
				   "the function does. ~~"),
			 "}"),
			 usage = c("\\usage{", paste0(Call, ")"), "}"),
			 arguments = NULL,
			 details = c("\\details{",
			 paste("  ~~ If necessary, more details than the",
				   "description above ~~"),
			 "}"),
			 value = c("\\value{",
			 "	~Describe the value returned",
			 "	If it is a LIST, use",
			 "	\\item{comp1 }{Description of 'comp1'}",
			 "	\\item{comp2 }{Description of 'comp2'}",
			 "	...",
			 "}"),
			 references = paste("\\references{ ~put references to the",
			 "literature/web site here ~ }"),
			 author = sprintf("\\author{ Mark Cowley }", format(Sys.Date(), "%d/%m/%Y")),
			 note = c("\\note{ ~~further notes~~ }",
					  "\\section{Warning }{....}"),
			 seealso = paste("\\seealso{",
			 "\\code{\\link{help}}, \\code{\\link[utils]{help}}, \\code{\\link{help}}, \\code{\\link{help}} }"),
			 examples = c("\\examples{",
			 "##---- Should be DIRECTLY executable !! ----",
			 "##-- ==>	Define data, use random, or do  help(data=index)  for the standard data sets.",
			 "}"),
			 keywords = "\\keyword{ ~kwd }"
			)

	Rdtxt$arguments <- if(n > 0)
		c("\\arguments{",
		  paste0("	\\item{", arg.n, "}{ ~~Describe \\code{", arg.n, "} here~~ }"),
		  "}") ## else NULL

	# overwrite the details section IF there was a header that i'd already written in the src file.
	hdr <- import.function.header(name, strip.comment.char=TRUE)
	if( length(hdr) > 0 ) {
		Rdtxt$details <- c("\\details{",hdr,"}")
		if( any(grepl("Cowley", hdr)) )
			Rdtxt$author <- sprintf("\\author{ %s }", hdr[grep("Cowley", hdr)[1]])
	}
	if( any(grepl("import", name)) || any(grepl("export", name)) || any(grepl("^read", name)) ) {
		Rdtxt$keywords <- paste0("\\keyword{ file }", "\\keyword{ IO }")
	}

	if(is.na(filename)) return(Rdtxt)

	cat(unlist(Rdtxt), file = filename, sep = "\n")

	message(gettextf("Created file named '%s'.", filename),
			"\n",
			gettext("Edit the file and move it to the appropriate directory."),
			domain = NA)

	invisible(filename)
}
# CHANGELOG
# 2012-07-06: moved from pwbc -> updateR
