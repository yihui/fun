\name{mineR}
\alias{mineR}
\title{Play the Mine game in R}
\description{
Use R to play the Mine game (currently only for Windows system).
}
\usage{
mineR(width = 10, height = 10, mines = 20, text.cex = 2, 
      cheat = FALSE, seed = NULL, ...)
}
\arguments{
  \item{width}{ number of grids in horizontal axis. }
  \item{height}{ number of grids in vertical axis. }
  \item{mines}{ number of mines. }
  \item{text.cex}{
  the amount by which text in graphics should be magnified relative to the default. Adjust this parameter when the size of text doesn't fit the grid. 
}
  \item{cheat}{ logical. If \code{TRUE} a matrix indicating the mines will be printed. }
  \item{seed}{ seed for random number generator. }
  \item{\dots}{ other arguments passed to \code{\link[base:Random]{set.seed}}. }
}
\details{
The controls should be familiar to you: Click the left mouse button to dig an area, and right button to mark or unmark the area with flags.
}
\author{ Yixuan Qiu }
\examples{
\dontrun{
mineR(width=8, height=8, mines=10, text.cex=3)
}
}
\keyword{ iplot }
