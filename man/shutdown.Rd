\name{shutdown}
\alias{shutdown}
\title{Shut down the operating system with the command `shutdown'}
\usage{
shutdown(wait = 0)
}
\arguments{
  \item{wait}{time to wait before shutting down (in
  seconds); passed to \code{\link[base]{Sys.sleep}}}
}
\value{
  The status code of \code{\link[base]{system}}.
}
\description{
  There is a command \command{shutdown} in both Windows and
  Linux, and this function uses it to shut down a computer.
}
\details{
  After the time \code{wait} has passed, R will execute
  \command{shutdown -s -t 0} (for Windows) or
  \command{shutdown -h now} to shut down the computer.
}
\examples{
\dontrun{
## when your code is extremely time-consuming, you may need
#   this function; e.g. you go to sleep, and R is running
#   long computation... complex graphics... and long long
#   computation... at last,
shutdown()
## the next day you wake up, 'thank you, R' :)
}
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\references{
  \url{http://cos.name/en/topic/shut-down-your-windows-with-r}
}
\seealso{
  \code{\link[base]{system}}, \code{\link[base]{Sys.sleep}}
}
