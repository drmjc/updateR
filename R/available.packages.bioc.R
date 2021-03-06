#' Download a current list of available CRAN packages
#'
#' This is a wrapper around available.packages, with default options set.
#'
#' @param prefix the URL prefix, eg: \url{http://mirror.aarnet.edu.au/pub/CRAN}
#' @return
#' Same as the output from \code{\link[utils]{available.packages}}, but only listing
#' packages from CRAN: A \code{matrix} with one row per package, row names the package names and
#'   column names \dQuote{Package}, \dQuote{Version}, \dQuote{Priority}, \dQuote{Depends},
#'   \dQuote{Imports}, \dQuote{LinkingTo}, \dQuote{Suggests}, \dQuote{Enhances},
#'   \dQuote{OS_type}, \dQuote{License}, \dQuote{File} and \dQuote{Repository}.  Additional
#'   columns can be specified using the \sQuote{fields} argument.
#' @seealso \code{\link[utils]{available.packages}}
#' @export
#' @author Mark Cowley
#' @examples
#' options(repos="http://mirror.aarnet.edu.au/pub/CRAN")
#' p <- available.packages.cran()
#' str(p)
available.packages.cran <- function(prefix=getOption("repos")) {
	available.packages(contrib.url(prefix, "source"))
}
# CHANGELOG
# 2011-04-07: v1


#' Download a current list of available Bioconductor packages
#' 
#' This is a wrapper around \dQuote{available.packages}, with default options set.
#' @param prefix the URL prefix, eg: \url{http://mirror.aarnet.edu.au/pub/CRAN}
#' @return
#' Same as the output from \code{\link[utils]{available.packages}}, but only listing
#' packages from Bioconductor: A \code{matrix} with one row per package, row names the package names and
#'   column names \dQuote{Package}, \dQuote{Version}, \dQuote{Priority}, \dQuote{Depends},
#'   \dQuote{Imports}, \dQuote{LinkingTo}, \dQuote{Suggests}, \dQuote{Enhances},
#'   \dQuote{OS_type}, \dQuote{License}, \dQuote{File} and \dQuote{Repository}.  Additional
#'   columns can be specified using the \sQuote{fields} argument.
#' @seealso \code{\link[utils]{available.packages}}
#' @author Mark Cowley
#' @export 
#' @examples
#' # options(BioC_mirror="http://mirror.aarnet.edu.au/pub/bioconductor")
#' tmp <- available.packages.bioc()
#' str(tmp)
available.packages.bioc <- function(prefix=getOption("BioC_mirror")) {
	# This works OK but can be noisy
	# source("http://bioconductor.org/biocLite.R")
	# bioc.urls <- biocinstallRepos()
	# bioc.urls <- bioc.urls[-grep("cran", names(bioc.urls), ignore.case=TRUE)]
	# bioc.urls <- bioc.urls[-grep("cran", bioc.urls, ignore.case=TRUE)]
	# bioc.urls <- paste(bioc.urls, "/src/contrib", sep="")
	Biocver <- bioc.version()
	bioc.urls <- paste(
		prefix, 
		sprintf(c(
			"/packages/%s/bioc/src/contrib",
			"/packages/%s/data/annotation/src/contrib",
			"/packages/%s/data/experiment/src/contrib",
			"/packages/%s/extra/src/contrib"
		 ), Biocver, Biocver, Biocver, Biocver),
		sep=""
	)
	bioc.urls <- c(bioc.urls, "http://brainarray.mbni.med.umich.edu/bioc/src/contrib")

	bioc <- lapply(bioc.urls, available.packages)
	res <- do.call(rbind, bioc)
	res
}
# CHANGELOG
# 2011-04-07: v1


#' What version of Bioconductor does this version of R use?
#'
#' Since at least R 2.5, Bioc has been 5 minor points behind, so
#' R 2.5 = bioc 2.0
#' R 2.12 = bioc 2.7 ...
#'
#' @return The current version of bioc as a character string, eg "2.7"
#' @author Mark Cowley
#' @export
#' @examples
#' bioc.version()
bioc.version <- function() {
	Rver <- c( as.numeric(R.Version()$major), floor(as.numeric(R.Version()$minor)) )
	Biocver <- Rver - c(0,5)
	Biocver <- paste(Biocver[1], Biocver[2], sep=".")
	Biocver
}
# CHANGELOG 
# 2011-04-07: v1
