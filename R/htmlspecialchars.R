htmlspecialchars <-
function(string) {
    x = c("&", "\"", "'", "<", ">")
    subx = c("&amp;", "&quot;", "&#039;", "&lt;", "&gt;")
    for (i in seq_along(x)) {
        string = gsub(x[i], subx[i], string, fixed = TRUE)
    }
    string
}

