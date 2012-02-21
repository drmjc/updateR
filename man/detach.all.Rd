\name{detach.all}
\alias{detach.all}
\title{Detach all user-loaded packages}
\usage{
  detach.all(keep = getOption("defaultPackages"))
}
\arguments{
  \item{keep}{a vector of packages to not detach. Defaults
  to the packages that are automatically loaded upon system
  start, as well as \code{Autoloads}, \code{base}, and the
  \code{.GlobalEnv}}
}
\value{
  invisibly returns the vector of package names that are
  detached
}
\description{
  Detach all user-loaded packages
}
\examples{
\dontrun{
detach.all()
}
}
\author{
  Mark Cowley, 2011-10-19
}
\seealso{
  \code{\link{detach}} \code{\link{options}}
}

