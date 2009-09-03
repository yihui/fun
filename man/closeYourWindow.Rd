\name{closeYourWindow}
\alias{closeYourWindow}
\title{Play the Close Your Window game in R}
\description{
Use R to play the Close Your Window game (currently only for Windows system).
}
\usage{
closeYourWindow(width = 5, height = 5, steps = 3, cheat = FALSE, 
                col.closed = "black", col.open = "white", 
                col.frame = "lightblue", seed = NULL)
}
\arguments{
  \item{width}{ number of grids in horizontal axis. }
  \item{height}{ number of grids in vertical axis. }
  \item{steps}{ number of "seed" grids to initialize the puzzle. In general, the larger \code{steps} is, the more complex this puzzle may be. }
  \item{cheat}{ logical. If \code{TRUE} a data frame indicating the steps to solve this puzzle will be printed. }
  \item{col.closed}{ color of closed window. }
  \item{col.open}{ color of open window. }
  \item{col.frame}{ color of window frame. }
  \item{seed}{ seed for random number generator. }
  \item{\dots}{ other arguments passed to \code{\link[base:Random]{set.seed}}. }
}
\details{
In default, the white grids in the graphics stand for the windows that are open, and black ones for the closed. When you click on a window, this window as well as the four ones next to it will change its status. Your mission is to close all the windows.
}
\author{ Yixuan Qiu }
\examples{
\dontrun{
closeYourWindow(width=5, height=5, steps=3)
}
}
\keyword{ window }
