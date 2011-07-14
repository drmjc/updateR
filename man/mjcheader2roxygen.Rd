\name{mjcheader2roxygen}
\alias{mjcheader2roxygen}
\title{Parse a function header comment into roxygen list...}
\usage{mjcheader2roxygen(hdr, docType="", format="", keywords="", default.author="Mark Cowley")
}
\description{Parse a function header comment into roxygen list}
\seealso{\code{\link[Rd2roxygen]{parse_file}}, \code{\link{import.function.header}}, \code{\link{import.function.header.from.src.file}}}
\arguments{\item{hdr}{a character vector containing the parsed header.}
\item{docType}{default docType for the function}
\item{format}{default format for the function}
\item{keywords}{default keywords for the function}
\item{default.author}{the default author. This is ignored if author is set}
}
\examples{hdr <- import.function.header.from.src.file("mjcheader2roxygen", "~/src/R/updateR/R/mjc2roxygen.R")
rox <- mjcheader2roxygen(hdr)
str(rox)}

