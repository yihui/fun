##' Shut down the operating system with the command `shutdown'
##'
##' There is a command \command{shutdown} in both Windows and Linux,
##' and this function uses it to shut down a computer.
##'
##' After the time \code{wait} has passed, R will execute
##' \command{shutdown -s -t 0} (for Windows) or \command{shutdown -h
##' now} to shut down the computer.
##' @param wait time to wait before shutting down (in seconds); passed
##' to \code{\link[base]{Sys.sleep}}
##' @return The status code of \code{\link[base]{system}}.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link[base]{system}}, \code{\link[base]{Sys.sleep}}
##' @references \url{http://cos.name/en/topic/shut-down-your-windows-with-r}
##' @export
##' @examples
##' \dontrun{
##' ## when your code is extremely time-consuming, you may need this function; e.g. you go to sleep, and R is running long computation... complex graphics... and long long computation... at last,
##' shutdown()
##' ## the next day you wake up, "thank you, R" :)
##' }
shutdown = function(wait = 0) {
    Sys.sleep(wait)
    ifelse(.Platform$OS.type == "windows", shell("shutdown -s -t 0"),
        system("shutdown -h now"))
}
