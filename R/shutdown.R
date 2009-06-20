shutdown <-
function(wait = 0) {
    Sys.sleep(wait)
    shell(ifelse(.Platform$OS.type == "windows", "shutdown -s -t 0",
        "shutdown -h now"))
}

