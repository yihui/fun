shutdown <-
function(wait = 0) {
    Sys.sleep(wait)
    ifelse(.Platform$OS.type == "windows", shell("shutdown -s -t 0"),
        system("shutdown -h now"))
}

