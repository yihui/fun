\name{htmlspecialchars}
\Rdversion{1.1}
\alias{htmlspecialchars}
\title{
Replace HTML special characters with HTML entities.
}
\description{
The characters \code{c("&", "\"", "'", "<", ">")} will be replaced with \code{c("&amp;", "&quot;", "&#039;", "&lt;", "&gt;")} respectively.
}
\usage{
htmlspecialchars(string)
}
\arguments{
  \item{string}{
the string with (or w/o) HTML special chars
}
}
\value{
the string with special chars replaced.
}
\references{
\url{http://php.net/manual/en/function.htmlspecialchars.php}
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link[base]{gsub}}
}
\examples{
htmlspecialchars("<a href = 'http://yihui.name'>Yihui</a>")
# &lt;a href = &#039;http://yihui.name&#039;&gt;Yihui&lt;/a&gt;
}
\keyword{ manip }
