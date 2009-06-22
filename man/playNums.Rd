\name{playNums}
\Rdversion{1.1}
\alias{playNums}
\title{
Sliding puzzle in R
}
\description{
Use R to play sliding puzzle.
}
\usage{
playNums(size = NULL, bg = "lightblue", z = NULL, ...)
}

\arguments{
  \item{size}{
two dimensional vector, the size of sliding puzzle. Note: the element of \code{size} must be greater than 1.
}

  \item{bg}{
the background color of blocks.
}

  \item{z}{
the matrix of sliding puzzle, if z is specialized, \code{size} will be omited.
}

}

\details{
If \code{size} is specialized and \code{z} is \code{NULL}, then the function will generate a solvable sliding puzzle.
}

\references{
About the sliding puzzle: \url{http://gfxtalk.net/index.php?hl=f5&q=uggc%3A%2F%2Fra.jvxvcrqvn.bet%2Fjvxv%2FFyvqvat_chmmyr}

How to Solve a Slider Puzzle: \url{http://www.justadventure.com/articles/Slider/Slider.shtm}
}
\author{
Taiyun Wei
}


\examples{
\dontrun{ 
playNums(c(3,3))

playNums(z=matrix(0:11, 3, 4))
}
}
\keyword{sliding}
\keyword{puzzle}
