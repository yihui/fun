\name{tagData}
\Rdversion{1.1}
\alias{tagData}
\docType{data}
\title{
Tag Information of Yihui Xie's English Blog
}
\description{
Tag data collected from Yihui Xie's Blog, containing the tag words, frequency and hyperlinks, etc.
}
\usage{data(tagData)}
\format{
  A data frame with 45 observations on the following 5 variables.
  \describe{
    \item{\code{tag}}{a character vector; the tag words}
    \item{\code{link}}{a character vector; hyperlinks of tags}
    \item{\code{count}}{a numeric vector; the frequency of tags in blogs (see Details)}
    \item{\code{color}}{a character vector in hexadecimal format specifying the RGB component of tag colors}
    \item{\code{hicolor}}{a character vector similar to \code{color}; the color when mouse hangs over the tag}
  }
}
\details{
The \code{count} was multiplied by 4 in the data in order that the tag cloud could be more clear.
}
\source{
\url{http://yihui.name/en/} (accessed on June 10, 2009)
}
\examples{
data(tagData)
hist(tagData$count/4, 10)  # extremely right skewed
# see help(tagCloud) for the example of creating tag cloud with this data
}
\keyword{datasets}
