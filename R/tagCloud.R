tagCloud <-
function(tagData, htmlOutput = "tagCloud.html", 
    SWFPath, JSPath, divId = "tagCloudId", width = 600, height = 400, 
    transparent = FALSE, tcolor = "333333", tcolor2 = "009900", 
    hicolor = "ff0000", distr = "true", tspeed = 100, version = 9, 
    bgcolor = "ffffff", useXML = FALSE, htmlTitle = "Tag Cloud", 
    noFlashJS, target = NULL, scriptOnly = FALSE, encode = FALSE) {
    if (missing(SWFPath)) 
        SWFPath = "http://www.roytanck.com/wp-content/plugins/wp-cumulus/tagcloud.swf"
    if (missing(JSPath)) 
        JSPath = "http://www.roytanck.com/wp-content/plugins/wp-cumulus/swfobject.js"
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
        tagXML = URLencode(tagXML)
    if (useXML) 
        cat(tagXML, file = file.path(dirname(htmlOutput), "tagCloud.xml"))
    cat(ifelse(scriptOnly, "", sprintf("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n</head>\n\n<body>", 
        htmlTitle)), sprintf("\t<script type=\"text/javascript\" src=\"%s\"></script>", 
        JSPath), sprintf("\t<div id=\"%s\">%s</div>", divId, 
        noFlashJS), sprintf("\t<script type=\"text/javascript\">\n\t\tvar so = new SWFObject(\"%s\", \"tagcloud\", \"%d\", \"%d\", \"%d\", \"#%s\");\n%s\t\tso.addVariable(\"mode\", \"tags\");\n\t\tso.addVariable(\"tcolor\", \"0x%s\");\n\t\tso.addVariable(\"tcolor2\", \"0x%s\");\n\t\tso.addVariable(\"hicolor\", \"0x%s\");\n\t\tso.addVariable(\"tspeed\", \"%d\");\n\t\tso.addVariable(\"distr\", \"%s\");\n%s\n\t\tso.write(\"%s\");\n\t\t</script>\n", 
        SWFPath, width, height, version, bgcolor, ifelse(transparent, 
            "\t\tso.addParam(\"wmode\", \"transparent\");\n", 
            ""), tcolor, tcolor2, hicolor, tspeed, distr, ifelse(useXML, 
            "\t\tso.addVariable(\"xmlpath\", \"tagcloud.xml\");", 
            sprintf("\t\tso.addVariable(\"tagcloud\", \"%s\");", 
                tagXML)), divId), ifelse(scriptOnly, "", "</body>\n\n</html>"), 
        file = ifelse(scriptOnly, stdout(), htmlOutput), sep = "\n")
        if (!scriptOnly)
            cat("HTML file created at ", htmlOutput, "\n")
}

