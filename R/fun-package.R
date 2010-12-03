

##' Use R for Fun
##' This is a collection of R games and other funny stuff, such as the
##' classical Mine sweeper and sliding puzzles.
##'
##' \tabular{ll}{ Package: \tab fun\cr Type: \tab Package\cr Version: \tab
##' 0.1-0\cr Date: \tab 2009-04-07\cr License: \tab GPL-2 | GPL-3\cr LazyLoad:
##' \tab yes\cr }
##'
##' @name fun-package
##' @aliases fun-package fun
##' @docType package
##' @author Yihui Xie, Taiyun Wei and Yixuan Qiu
##' @keywords package
##' @examples
##'
##' # TODO
##'
NULL





##' Tag Information of Yihui Xie's English Blog
##' Tag data collected from Yihui Xie's Blog, containing the tag words,
##' frequency and hyperlinks, etc.
##'
##' The \code{count} was multiplied by 4 in the data in order that the tag
##' cloud could be more clear.
##'
##' @name tagData
##' @docType data
##' @format A data frame with 45 observations on the following 5 variables.
##'   \describe{ \item{list("tag")}{a character vector; the tag words}
##'   \item{list("link")}{a character vector; hyperlinks of tags}
##'   \item{list("count")}{a numeric vector; the frequency of tags in blogs
##'   (see Details)} \item{list("color")}{a character vector in hexadecimal
##'   format specifying the RGB component of tag colors}
##'   \item{list("hicolor")}{a character vector similar to \code{color}; the
##'   color when mouse hangs over the tag} }
##' @source \url{http://yihui.name/en/} (accessed on June 10, 2009)
##' @keywords datasets
##' @examples
##'
##' data(tagData)
##' hist(tagData$count/4, 10)  # extremely right skewed
##' # see help(tagCloud) for the example of creating tag cloud with this data
##'
NULL



