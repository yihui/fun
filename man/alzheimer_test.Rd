\name{alzheimer_test}
\alias{alzheimer_test}
\title{Test Alzheimer's disease by finding out the different character in
a character rectangle}
\usage{
alzheimer_test(char1 = c("9", "O", "M", "I", "F", 
    "D"), char2 = c("6", "C", "N", "T", "E", "O"), nr = 10, nc = 30, 
    seed = NULL, ...)
}
\arguments{
  \item{char1}{the 'background' character}

  \item{char2}{the character to be found out}

  \item{nr}{number of rows of the character rectangle}

  \item{nc}{number of columns}

  \item{seed}{seed for random number generation}

  \item{...}{other arguments passed to
  \code{\link[base]{set.seed}}}
}
\value{
  If at least one test item has been passed, a data.frame
  will be returned telling the result of the test.
}
\description{
  Please try hard to find the letter "N" in 300 "M"s, one
  "6" in 300 "9"s, etc.
}
\details{
  Follow the instructions and finish the test.
}
\note{
  Don't be too serious about this test. I'm no doctor, but
  I think this will be a good present to your friends.
}
\examples{
x = alzheimer_test()
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
