#' Shut down the operating system with the command `shutdown'
#'
#' There is a command \command{shutdown} in both Windows and Linux, and this
#' function uses it to shut down a computer.
#'
#' After the time \code{wait} has passed, R will execute \command{shutdown -s -t
#' 0} (for Windows) or \command{shutdown -h now} to shut down the computer.
#' @param wait time to wait before shutting down (in seconds); passed to
#'   \code{\link{Sys.sleep}}
#' @return The status code of \code{\link{system}}.
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso \code{\link{system}}, \code{\link{Sys.sleep}}
#' @export
#' @examples
#' if (interactive()) {
#' # when your code is extremely time-consuming, you may need this function; e.g. you go to sleep, and R is running long computation... complex graphics... and long long computation... at last,
#' shutdown()
#' # the next day you wake up, "thank you, R" :)
#' }
shutdown = function(wait = 0) {
  Sys.sleep(wait)
  if (.Platform$OS.type == "windows") {
    shell("shutdown -s -t 0", wait = FALSE)
  } else {
    system("shutdown -h now")
  }
}
