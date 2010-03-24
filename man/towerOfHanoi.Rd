\name{towerOfHanoi}
\Rdversion{1.1}
\alias{towerOfHanoi}
\title{
Demonstrate the Tower of Hanoi puzzle in R.
}
\description{
This function uses the recursive algorithm to solve the Tower of Hanoi puzzle, and demonstrates the game in animation.
}
\usage{
towerOfHanoi(n)
}

\arguments{
  \item{n}{
an integer indicating the number of disks on the rot.
}
}
\details{
This function was written by Linlin Yan <linlin.yan@cos.name> in a Chinese forum (See 'References') to show the usage of recursive algorithm.
}
\references{
Original code: \url{http://cos.name/cn/topic/101199}

About the Tower of Hanoi: \url{http://en.wikipedia.org/wiki/Tower_of_Hanoi}
}

\seealso{
\code{\link[graphics]{barplot}}
}
\examples{
\dontrun{ 
towerOfHanoi(7)
}
}
\keyword{ dynamic }
