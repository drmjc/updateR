\name{write_PACKAGES}
\alias{write_PACKAGES}
\alias{write_PACKAGES_dir}
\title{write PACKAGES file}
\usage{
  write_PACKAGES(pkgs, outfile = "~/src/contrib/PACKAGES")

  write_PACKAGES_dir(dir,
    outfile = "~/src/contrib/PACKAGES")
}
\arguments{
  \item{pkgs}{a character vector of package names}

  \item{outfile}{the path to the PACKAGES file. this should
  be within <mirror>/src/contrib/PACKAGES}

  \item{dir}{the path to a directory containing tar.gz
  bundles.}
}
\value{
  nothing. the outfile is written
}
\description{
  When hosting a local mirror of R packages, you need a
  PACKAGES file which summarises the packages that are
  being hosted. This writes that PACKAGES file.
}
\examples{
f <- tempfile()
write_PACKAGES(c("mjcbase", "mjcstats", "mjcgraphics", "excelIO"), f)
readLines(f)

\dontrun{
# all of my packages:
pkgs <- basename(system("find ~/src/R -maxdepth 2 -name DESCRIPTION -exec dirname {} \\\\;", intern=T))
write_PACKAGES(pkgs)
}
}
\author{
  Mark Cowley, 2013-01-30
}

