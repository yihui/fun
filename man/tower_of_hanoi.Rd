\name{tower_of_hanoi}
\alias{tower_of_hanoi}
\title{Demonstrate the Tower of Hanoi puzzle in R}
\usage{
tower_of_hanoi(n = 7)
}
\arguments{
  \item{n}{an integer indicating the number of disks on the
  rot.}
}
\description{
  This function uses the recursive algorithm to solve the
  Tower of Hanoi puzzle, and demonstrates the game in
  animation.
}
\details{
  This function was written by Linlin Yan
  <linlin.yan@cos.name> in a Chinese forum (See
  'References') to show the usage of recursive algorithm.
}
\examples{
tower_of_hanoi(7)
}
\author{
  Linlin Yan <\email{linlin.yan@cos.name}>
}
\references{
  Original code: \url{http://cos.name/cn/topic/101199}

  About the Tower of Hanoi:
  \url{http://en.wikipedia.org/wiki/Tower_of_Hanoi}
}
\seealso{
  \code{\link[graphics]{barplot}}
}
\keyword{dynamic}
