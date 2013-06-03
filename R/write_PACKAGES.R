#' write PACKAGES file
#' 
#' When hosting a local mirror of R packages, you need a PACKAGES
#' file which summarises the packages that are being hosted. This
#' writes that PACKAGES file.
#'
#' @param pkgs a character vector of package names
#' @param outfile the path to the PACKAGES file. this should be within <mirror>/src/contrib/PACKAGES
#' 
#' @return nothing. the outfile is written
#' 
#' @author Mark Cowley, 2013-01-30
#' @rdname write_PACKAGES
#' 
#' @export
#' @examples
#' f <- tempfile()
#' write_PACKAGES(c("mjcbase", "mjcstats", "mjcgraphics", "excelIO"), f)
#' readLines(f)
#' 
#' \dontrun{
#' # all of my packages:
#' pkgs <- basename(system("find ~/src/R -maxdepth 2 -name DESCRIPTION -exec dirname {} \\;", intern=T))
#' write_PACKAGES(pkgs)
#' }
#' 
write_PACKAGES <- function(pkgs, outfile="~/src/contrib/PACKAGES") {
	
	.write1 <- function(pkginfo, fs) {
		for(tag in names(pkginfo)) {
			if( !is.na(pkginfo[tag]) && nchar(pkginfo[tag]) > 0 ) {
				txt <- pkginfo[tag]
				txt <- gsub("\n", " ", txt)
				txt <- paste(tag, txt, sep=": ")
				write(strwrap(txt, exdent=8), fs)
			}
		}
		write("", fs)
	}
	
	OUT <- file(outfile, "w")
	for(pkg in pkgs) {
		pkginfo <- packageDescription(pkg, fields=c("Package", "Version", "Depends", "Imports", "Suggests", "License"))
		.write1(pkginfo, OUT)
	}
	close(OUT)
}
# write_PACKAGES(c("mjcbase", "mjcstats", "mjcgraphics", "excelIO"))
# pkgs <- basename(system("find ~/src/R -maxdepth 2 -name DESCRIPTION -exec dirname {} \\;", intern=T))
# write_PACKAGES(pkgs)

#' @param dir the path to a directory containing tar.gz bundles.
#' @export
#' @rdname write_PACKAGES
write_PACKAGES_dir <- function(dir, outfile="~/src/contrib/PACKAGES") {
	pkgs <- basename(dir(pattern="*tar.gz"))
	pkgs <- sub("_.*", "", pkgs)
	write_PACKAGES(pkgs, outfile)
}

