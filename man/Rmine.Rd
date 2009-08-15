\name{Rmine}
\alias{Rmine}
\title{Play the Mine game in R}
\description{
Use R to play the Mine game (currently only for Windows system).
}
\usage{
Rmine(width = 10, height = 10, mines = 20, text.cex = 2, 
      cheat = FALSE)
}
\arguments{
  \item{width}{ number of grids in horizontal axis }
  \item{height}{ number of grids in vertical axis }
  \item{mines}{ number of mines }
  \item{text.cex}{
  the amount by which text in graphics should be magnified relative to the default. Adjust this parameter when the size of text doesn't fit the grid. 
}
  \item{cheat}{ logical. If \code{TRUE} a matrix indicating the mines will be printed. }
}
\details{
Click the left mouse button to dig an area, and right button to mark or unmark flags.
}
\author{ Yixuan Qiu }
\examples{
\dontrun{
Rmine(width=8, height=8, mines=10, text.cex=3)
}
}
\keyword{ mine }
