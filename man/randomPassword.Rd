\name{randomPassword}
\Rdversion{1.1}
\alias{randomPassword}
\title{
Generate A Random Password with A Specified Length
}
\description{
This function generates a random passord sampled from the ASCII table.
}
\usage{
randomPassword(length = 12, replace = FALSE)
}
\arguments{
  \item{length}{
length of the password
}
  \item{replace}{
sample from the ASCII table with (\code{TRUE}) or without (\code{FALSE}) replacement?
}
}
\value{
a character string
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\references{
\url{http://cos.name/en/topic/generating-passwords-with-r-from-ascii-characters}
}
\seealso{
\code{\link[base]{sample}}
}
\examples{
# I don't know the result
randomPassword()
# set the seed (you get "<jFqu%OnPG{|")
set.seed(123)
randomPassword()
# long password
randomPassword(20, TRUE)
}
\keyword{ misc }

