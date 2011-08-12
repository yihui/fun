##' Use R for Fun
##'
##' This is a collection of R games and other funny stuff, such as the
##' classical Mine sweeper and sliding puzzles.
##'
##' \tabular{ll}{ Package: \tab fun\cr Type: \tab Package\cr License:
##' \tab GPL\cr LazyLoad: \tab yes\cr }
##'
##' New games are always welcome; if you know GIT and want to join the
##' development, please go to \url{https://github.com/yihui/fun}; or
##' simply contribute ideas at
##' \url{https://github.com/yihui/fun/issues}.
##' @name fun-package
##' @aliases fun
##' @docType package
##' @author Yihui Xie, Taiyun Wei and Yixuan Qiu
##' @keywords package
##' @examples ## See the examples in each function, or check out the demos
##' demo(package = 'fun')
NULL


##' Tag information of Yihui Xie's English blog
##'
##' Tag data collected from Yihui Xie's Blog, containing the tag
##' words, frequency and hyperlinks, etc.
##'
##' The \code{count} was multiplied by 4 in the data in order that the
##' tag cloud could be more clear.
##'
##' @name tagData
##' @docType data
##' @format A data frame with 45 observations on the following 5
##' variables.  \describe{ \item{tag}{a character vector; the tag
##' words} \item{link}{a character vector; hyperlinks of tags}
##' \item{count}{a numeric vector; the frequency of tags in blogs (see
##' Details)} \item{color}{a character vector in hexadecimal format
##' specifying the RGB component of tag colors} \item{hicolor}{a
##' character vector similar to \code{color}; the color when mouse
##' hangs over the tag} }
##' @source \url{http://yihui.name/en/} (accessed on June 10, 2009)
##' @keywords datasets
##' @examples
##' data(tagData)
##' hist(tagData$count/4, 10)  # extremely right skewed
##' # see help(tag_cloud) for the example of creating tag cloud with this data
NULL
