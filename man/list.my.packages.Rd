\name{list.my.packages}
\alias{list.my.packages}
\title{List my R packages which all sit inside src.root}
\usage{
  list.my.packages(src.root = getOption("src.root"),
    maxdepth = 1, only.installed = TRUE)
}
\arguments{
  \item{src.root}{The root folder containing all R package
  code.}

  \item{maxdepth}{do you only want to look into the folder
  1 below src.root, or further? defaults to 1.}

  \item{only.installed}{if TRUE, then only return packages
  which are also installed. if FALSE, then all candidate
  packages are returned.}
}
\value{
  A vector of package names
}
\description{
  Parameters:
}
\examples{
list.my.packages()
list.my.packages(only.installed=FALSE)
}
\author{
  Mark Cowley, 2011-04-07
}

