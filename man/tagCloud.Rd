\name{tagCloud}
\Rdversion{1.1}
\alias{tagCloud}
\title{
Creating Tag Cloud in R (with Flash and JavaScript)
}
\description{
Use R to write tag data (tag words, frequency, hyperlinks and colors, etc) into JavaScript, and the JavaScript code will generate a Flash movie. Finally the tag cloud can be created with fantastic 3D rotation effect.
}
\usage{
tagCloud(tagData, htmlOutput = "tagCloud.html", SWFPath, JSPath, 
    divId = "tagCloudId", width = 600, height = 400, transparent = FALSE, 
    tcolor = "333333", tcolor2 = "009900", hicolor = "ff0000", distr = "true", 
    tspeed = 100, version = 9, bgcolor = "ffffff", useXML = FALSE, 
    htmlTitle = "Tag Cloud", noFlashJS, target = NULL, scriptOnly = FALSE, 
    encode = FALSE)
}

\arguments{
  \item{tagData}{
a data.frame containing at least 3 columns: \code{tag}, \code{link} and \code{count}. Optional columns are \code{color} and \code{hicolor}.
}
  \item{htmlOutput}{
filename of the HTML output.
}
  \item{SWFPath}{
path of the SWF source file (\file{tagcloud.swf})
}
  \item{JSPath}{
path of the JavaScript file (\file{swfobject.swf})
}
  \item{divId}{
id of the tag cloud div (HTML layer)
}
  \item{width, height}{
width and height of the tag cloud
}
  \item{transparent}{
logical; whether to use transparent backgroud for the Flash movie?
}
  \item{tcolor, tcolor2, hicolor, distr, tspeed}{
see Details.
}
  \item{version}{
the required Flash version
}
  \item{bgcolor}{
backgroud color of the Flash movie
}
  \item{useXML}{
use XML file for the tag information or just a string; this will be passed to the Flash object as a variable
}
  \item{htmlTitle}{
title of the HTML file
}
  \item{noFlashJS}{
text to show if Flash or JavaScript is not supported
}
  \item{target}{
target window of the hyperlinks; possible values are \code{NULL}, \code{'_blank'}, \code{'_top'}, etc
}
  \item{scriptOnly}{
print the script in the console only? (if \code{TRUE}), no HTML file will be generated
}
  \item{encode}{
encode the tag XML or not? (with \code{\link[utils]{URLencode}}) set it to be \code{TRUE} when your browser does not recognize the tag XML correctly
}
}
\details{
This function is based on the WordPress plugin ``wp-cumulus''. If there are any arguments you don't understand, please check the reference.
}
\value{
\code{NULL}
}
\references{
About the WordPress plugin: \url{http://www.roytanck.com/2008/03/15/wp-cumulus-released/}

Explanation of some arguments: \url{http://www.roytanck.com/2008/05/19/how-to-repurpose-my-tag-cloud-flash-movie/}

Usage of the SWFObject: \url{http://blog.deconcept.com/swfobject/}

An example of visualizing tags in my blog: \url{http://yihui.name/en/2009/06/creating-tag-cloud-using-r-and-flash-javascript-swfobject/}
}
\author{
Yihui Xie
}

\seealso{
\code{\link[base]{cat}}, \code{\link[base]{sprintf}}, \code{\link[utils]{URLencode}}
}
\examples{
data(tagData)
htmlFile = sprintf("%s.html", tempfile())
if (file.create(htmlFile)) {
    tagCloud(tagData, htmlFile)
    browseURL(htmlFile)
}
}
\keyword{ dynamic }
\keyword{ file }
