\name{edit.src.file}
\alias{edit.src.file}
\alias{kate}
\alias{mate}
\alias{smultron}
\alias{textmate}
\alias{tm}
\title{edit an R source file}
\usage{
  edit.src.file(func, src.root = getOption("src.root"),
    src.files = NULL, editor = getOption("editor"),
    exclude.patterns = c("~", ".Rcheck", ".git", ".svn"))

  kate(func, src.root = getOption("src.root"))

  textmate(func, src.root = getOption("src.root"))

  tm(func, src.root = getOption("src.root"))

  mate(func, src.root = getOption("src.root"))

  smultron(func, src.root = getOption("src.root"))
}
\arguments{
  \item{func}{the quoted function name, or the function's
  code}

  \item{src.root}{The parent folder of source code}

  \item{src.files}{an optional vector of filenames to
  search within}

  \item{editor}{the path to your editor. Default =
  \code{getOption("editor")}}

  \item{exclude.patterns}{vector of patterns passed to grep
  for files to exclude.}
}
\value{
  nothing. The side effect is that an R source file is
  opened in an editor.
}
\description{
  Search for the .R file that contains a function, and open
  it in an editor. It searches for the file containing the
  function definition, by grepping for the pattern
  \code{"^my.func.name <- function"}, where
  \code{my.func.name} is the name of func.
}
\details{
  There are functions which work with textmate on OSX
  (textmate, tm, mate; smultron), or on Linux (kate = KDE
  Advanced Text Editor).

  Common usage scenarios are that you have a folder full of
  R packages under development. In this case, set
  \code{src.root}, or better yet, the
  \code{options(src.root="/path/to/source/root")} to point
  to the top level folder containing all the R packages. If
  you also have a directory full of .R files, then you can
  provide these files via the \code{src.files} argument.
}
\examples{
\dontrun{
edit.src.file("edit.src.file", "~/src/R")
edit.src.file("edit.src.file", "~/src/R", editor="open -a TextMate")
}
}
\author{
  Mark Cowley, 26/2/07
}
\seealso{
  \code{\link{find.src.file}}
}

