##' Creating Tag Cloud in R (with Flash and JavaScript)
##'
##' Use R to write tag data (tag words, frequency, hyperlinks and
##' colors, etc) into JavaScript, and the JavaScript code will
##' generate a Flash movie.  Finally the tag cloud can be created with
##' fantastic 3D rotation effect.
##'
##' This function is based on the WordPress plugin ``wp-cumulus''. If
##' there are any arguments you don't understand, please check the
##' reference.
##' @param tagData a data.frame containing at least 3 columns:
##' \code{tag}, \code{link} and \code{count}. Optional columns are
##' \code{color} and \code{hicolor}
##' @param htmlOutput filename of the HTML output
##' @param SWFPath path of the SWF source file (\file{tagcloud.swf});
##' see \code{system.file("js", "tagcloud.swf", package = "fun")}
##' @param JSPath path of the JavaScript file (\file{swfobject.js});
##' see \code{system.file("js", "swfobject.js", package = "fun")}
##' @param divId id of the tag cloud div (HTML layer)
##' @param width,height width and height of the tag cloud
##' @param transparent logical; whether to use transparent backgroud
##' for the Flash movie?
##' @param tcolor,tcolor2,hicolor,distr,tspeed see Details
##' @param version the required Flash version
##' @param bgcolor backgroud color of the Flash movie
##' @param useXML use XML file for the tag information or just a
##' string; this will be passed to the Flash object as a variable
##' @param htmlTitle title of the HTML file
##' @param noFlashJS text to show if Flash or JavaScript is not supported
##' @param target target window of the hyperlinks; possible values are
##' \code{NULL}, \code{'_blank'}, \code{'_top'}, etc
##' @param scriptOnly print the script in the console only? (if
##' \code{TRUE}), no HTML file will be generated
##' @param encode encode the tag XML or not? (with
##' \code{\link[utils]{URLencode}}) set it to be \code{TRUE} when your
##' browser does not recognize the tag XML correctly
##' @param reserved should reserved characters be encoded? see
##' \code{\link[utils]{URLencode}}
##' @return \code{NULL}
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link[base]{cat}}, \code{\link[base]{sprintf}},
##' \code{\link[utils]{URLencode}}
##' @references About the WordPress plugin:
##' \url{http://www.roytanck.com/2008/03/15/wp-cumulus-released/}
##'
##' Explanation of some arguments:
##' \url{http://www.roytanck.com/2008/05/19/how-to-repurpose-my-tag-cloud-flash-movie/}
##'
##' Usage of the SWFObject: \url{http://blog.deconcept.com/swfobject/}
##'
##' An example of visualizing tags in my blog:
##' \url{http://yihui.name/en/2009/06/creating-tag-cloud-using-r-and-flash-javascript-swfobject/}
##' @keywords dynamic file
##' @export
##' @examples
##' data(tagData)
##' htmlFile = paste(tempfile(), ".html", sep = "")
##' if (file.create(htmlFile)) {
##'     tag_cloud(tagData, htmlFile)
##'     if (!interactive()) browseURL(htmlFile)
##' }
tag_cloud = function(tagData, htmlOutput = "tagCloud.html",
    SWFPath, JSPath, divId = "tagCloudId", width = 600, height = 400,
    transparent = FALSE, tcolor = "333333", tcolor2 = "009900",
    hicolor = "ff0000", distr = "true", tspeed = 100, version = 9,
    bgcolor = "ffffff", useXML = FALSE, htmlTitle = "Tag Cloud",
    noFlashJS, target = NULL, scriptOnly = FALSE, encode = FALSE,
    reserved = FALSE) {
    tagData$tag = htmlspecialchars(tagData$tag)
    missingSWF = missing(SWFPath)
    if (missingSWF)
        SWFPath = "http://yihui.name/en/wp-content/uploads/2009/06/tagcloud.swf"
    if (missingSWF & useXML)
        SWFPath = "tagcloud.swf"
    if (missing(JSPath))
        JSPath = "http://yihui.name/en/wp-content/uploads/2009/06/swfobject.js"
    if (missing(noFlashJS))
        noFlashJS = "This will be shown to users with no Flash or Javascript."
    tagXML = sprintf("<tags>%s</tags>", paste(sprintf("<a href='%s' style='%s'%s%s%s>%s</a>",
        tagData$link, tagData$count, if (is.null(target))
            ""
        else sprintf(" target='%s'", target), if (is.null(tagData$color))
            ""
        else ifelse(!is.na(tagData$color), sprintf(" color='0x%s'",
            tagData$color, ""), ""), if (is.null(tagData$hicolor))
            ""
        else ifelse(!is.na(tagData$hicolor), sprintf(" hicolor='0x%s'",
            tagData$hicolor, ""), ""), tagData$tag), collapse = ""))
    if (encode)
        tagXML = URLencode(tagXML, reserved)
    if (useXML) {
        cat(tagXML, file = file.path(dirname(htmlOutput), "tagcloud.xml"))
        if (!("tagcloud.swf" %in% list.files(dirname(htmlOutput)))) {
            warning("The Flash file 'tagcloud.swf' must be under the same path with your XML file 'tagcloud.xml'!",
                immediate. = TRUE)
            SWFurl = "http://yihui.name/en/wp-content/uploads/2009/06/tagcloud.swf"
            if (tolower(substr(readline("Do you want to download the Flash file automatically? (y/n) "), 1, 1) == "y")) {
                download.file(SWFurl, file.path(dirname(htmlOutput), "tagcloud.swf"), mode = "wb")
            }
        }
    }
    cat(ifelse(scriptOnly, "", sprintf("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n</head>\n\n<body>",
        htmlTitle)), sprintf("\t<script type=\"text/javascript\" src=\"%s\"></script>",
        JSPath), sprintf("\t<div id=\"%s\">%s</div>", divId,
        noFlashJS), sprintf("\t<script type=\"text/javascript\">\n\t\tvar so = new SWFObject(\"%s\", \"tagcloud\", \"%d\", \"%d\", \"%d\", \"#%s\");\n%s%s\t\tso.addVariable(\"tcolor\", \"0x%s\");\n\t\tso.addVariable(\"tcolor2\", \"0x%s\");\n\t\tso.addVariable(\"hicolor\", \"0x%s\");\n\t\tso.addVariable(\"tspeed\", \"%d\");\n\t\tso.addVariable(\"distr\", \"%s\");\n%s\n\t\tso.write(\"%s\");\n\t\t</script>\n",
        SWFPath, width, height, version, bgcolor, ifelse(transparent,
            "\t\tso.addParam(\"wmode\", \"transparent\");\n",
            ""), ifelse(useXML, "", "\t\tso.addVariable(\"mode\", \"tags\");\n"),
            tcolor, tcolor2, hicolor, tspeed, distr, ifelse(useXML,
            "\t\tso.addVariable(\"xmlpath\", \"tagcloud.xml\");",
            sprintf("\t\tso.addVariable(\"tagcloud\", \"%s\");",
                tagXML)), divId), ifelse(scriptOnly, "", "</body>\n\n</html>"),
        file = ifelse(scriptOnly, stdout(), htmlOutput), sep = "\n")
        if (!scriptOnly)
            cat("HTML file created at ", htmlOutput, "\n")
}
