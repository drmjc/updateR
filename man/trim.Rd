\name{trim}
\alias{trim}
\title{Trim the whitespace from front and back of the words in a vector.}
\usage{trim(x)
}
\description{Trim the whitespace from front and back of the words in a vector.}
\value{a character vector of trimmed text}
\author{Mark Cowley, 2009-08-19}
\arguments{\item{x}{a character vector}
}
\examples{trim("   ABC")
# "ABC"
trim("DEF   ")
# "DEF"
trim("  ABC  ")
# "ABC"}

