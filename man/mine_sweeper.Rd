\name{mine_sweeper}
\alias{mine_sweeper}
\title{Play the Miner game in R}
\usage{
mine_sweeper(width = 10, height = 10, mines = 20, 
    text.cex = 2, cheat = FALSE, seed = NULL)
}
\arguments{
  \item{width}{number of grids in horizontal axis}

  \item{height}{number of grids in vertical axis}

  \item{mines}{number of mines}

  \item{text.cex}{the amount by which text in graphics
  should be magnified relative to the default. Adjust this
  parameter when the size of text doesn't fit the grid}

  \item{cheat}{logical. If \code{TRUE} a matrix indicating
  the mines will be printed}

  \item{seed}{seed for random number generator}
}
\description{
  The controls should be familiar to you: Click the left
  mouse button to dig in an area, and right button to mark
  or unmark the area with flags.
}
\note{
  Linux/Mac users have to use \code{X11(type = 'Xlib')} or
  the Cairo graphics device \code{Cairo()} in the package
  \pkg{cairoDevice}.
}
\examples{
## should use Xlib for the x11() device under *nix, e.g
\dontrun{
if (.Platform$OS.type == "windows") 
    x11() else x11(type = "Xlib")
}

mine_sweeper()
}
\author{
  Yixuan Qiu \email{yixuan.qiu@cos.name}
}
\references{
  \url{http://en.wikipedia.org/wiki/Minesweeper_(computer_game)}
}
\keyword{iplot}
