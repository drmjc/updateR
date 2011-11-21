\name{mjcheader2roxygen}
\alias{mjcheader2roxygen}
\title{Parse a function header comment into roxygen list}
\usage{
  mjcheader2roxygen(hdr, docType = "", format = "",
    keywords = "", default.author = "Mark Cowley")
}
\arguments{
  \item{hdr}{a character vector containing the parsed
  header.}

  \item{docType}{default docType for the function}

  \item{format}{default format for the function}

  \item{keywords}{default keywords for the function}

  \item{default.author}{the default author. This is ignored
  if author is set}
}
\value{
  a roxygenized named list, with names: \sQuote{desc},
  \sQuote{params}, \sQuote{details}, \sQuote{examples},
  \sQuote{usage}, \sQuote{author}, \sQuote{value},
  \sQuote{docType}, \sQuote{format}, \sQuote{note},
  \sQuote{aliases}, \sQuote{keywords}.
}
\description{
  Parse a function header comment into roxygen list
}
\examples{
hdr <- import.function.header.from.src.file("mjcheader2roxygen", "~/src/R/updateR/R/mjc2roxygen.R")
rox <- mjcheader2roxygen(hdr)
str(rox)
\dontrun{
# copy an mjc-style comment header from a src file
hdr <- pbpaste()
x <- Rd2roxygen::create_roxygen(mjcheader2roxygen(hdr))
writeLines(x, pipe("pbcopy", "w"))
}
}
\seealso{
  \code{\link[Rd2roxygen]{parse_file}},
  \code{\link{import.function.header}},
  \code{\link{import.function.header.from.src.file}}
}

