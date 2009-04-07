\name{alzheimer.test}
\alias{alzheimer.test}
\title{ Test Alzheimer's Disease by Finding out the Different Character in A Character Rectangle}
\description{
  Please try hard to find the letter "N" in 300 "M"s, one "6" in 300 "9"s, etc.
}
\usage{
alzheimer.test(char1 = c("9", "O", "M", "I", "F", "D"), 
    char2 = c("6", "C", "N", "T", "E", "O"), nr = 10, nc = 30, 
    seed = NULL, ...)
}
\arguments{
  \item{char1}{ the 'background' character }
  \item{char2}{ the character to be found out }
  \item{nr}{ number of rows of the character rectangle }
  \item{nc}{ number of columns }
  \item{seed}{ seed for random number generation }
  \item{\dots}{ other arguments passed to \code{\link[base:Random]{set.seed}} }
}
\details{
  Follow the guidelines and finish the test.
}
\value{
  If at least one test item has been passed, a data.frame will be returned telling the result of the test.
}
\author{ Yihui Xie }
\note{ 
Don't be too serious about this test. I'm no doctor, but I think this will be a good present to your friends.
}
\examples{
\dontrun{
if (interactive()) x = alzheimer.test()
}
}
\keyword{ print }
