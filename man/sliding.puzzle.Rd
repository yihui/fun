\name{sliding.puzzle}
\Rdversion{1.1}
\alias{sliding.puzzle}
\title{
Sliding puzzle in R
}
\description{
Use R to play sliding puzzle (currently only the Windows screen display).
}
\usage{
sliding.puzzle(size = NULL, bg = "lightblue", z = NULL, ...)
}

\arguments{
  \item{size}{
two dimensional vector, the size of sliding puzzle. Note: the element of \code{size} must be greater than 1.
}

  \item{bg}{
the background color of blocks.
}

  \item{z}{
the matrix of sliding puzzle, if z is specified, \code{size} will be omited.
}

}

\details{
If \code{size} is specified and \code{z} is \code{NULL}, then the function will generate a solvable sliding puzzle. In addition, the function only works under the Windows screen display because of the limitation of function \code{getGraphicsEvent}.
}

\references{
About the sliding puzzle: \url{http://en.wikipedia.org/wiki/Sliding_puzzle}

How to Solve a Slider Puzzle: \url{http://www.justadventure.com/articles/Slider/Slider.shtm}
}
\author{
Taiyun Wei
}


\examples{
\dontrun{ 
sliding.puzzle(c(3,3))

sliding.puzzle(z=matrix(0:11, 3, 4))
}
}
\keyword{iplot}
