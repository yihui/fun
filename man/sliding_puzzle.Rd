\name{sliding_puzzle}
\alias{sliding_puzzle}
\title{Sliding puzzle in R}
\usage{
sliding_puzzle(size = c(3, 3), bg = "lightblue", z = NULL)
}
\arguments{
  \item{size}{two dimensional vector, the size of sliding
  puzzle. Note: the element of \code{size} must be greater
  than 1.}

  \item{bg}{the background color of blocks.}

  \item{z}{the matrix of sliding puzzle, if z is specified,
  \code{size} will be omited.}
}
\description{
  Use R to play sliding puzzle (currently only the Windows
  screen display).
}
\details{
  If \code{size} is specified and \code{z} is \code{NULL},
  then the function will generate a solvable sliding
  puzzle. In addition, the function only works under the
  Windows screen display because of the limitation of
  function \code{getGraphicsEvent}.
}
\note{
  Linux/Mac users have to use
  \code{\link[grDevices]{X11}(type = 'Xlib')} or the Cairo
  graphics device \code{Cairo()} in the package
  \pkg{cairoDevice}.
}
\examples{
## should use Xlib for the x11() device under *nix, e.g
\dontrun{
if (.Platform$OS.type == "windows") 
    x11() else x11(type = "Xlib")
}

sliding_puzzle()

sliding_puzzle(z = matrix(0:11, 3, 4))
}
\author{
  Taiyun Wei
}
\references{
  About the sliding puzzle:
  \url{http://en.wikipedia.org/wiki/Sliding_puzzle}

  How to Solve a Slider Puzzle:
  \url{http://www.justadventure.com/articles/Slider/Slider.shtm}
}
\keyword{iplot}
