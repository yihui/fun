##' The game of Gomoku, a.k.a Five in a row
##'
##' There are two players in this game who play one after the other
##' using black and white stones respectively. The winner is the first
##' player to get an unbroken row of five stones horizontally,
##' vertically, or diagonally.
##' @param n the number of rows and columns in the board (the default
##' 19 generates the standard board)
##' @return \code{NULL}
##' @note The players should judge the winner by themselves; this
##' function does not do this job (patches are welcome, of course).
##' @author Yihui Xie <\url{http://yihui.name}>; modified from the
##' code by pklin
##' @references \url{http://cos.name/cn/topic/104750},
##' \url{http://en.wikipedia.org/wiki/Gomoku}
##' @export
##' @examples gomoku()
gomoku <- function(n = 19) {
    if (!interactive()) return()
    par(mar = rep(0, 4))
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    segments(1, 1:n, n, 1:n)
    segments(1:n, 1, 1:n, n)
    points(rep(c(4, 10, 16), 3), rep(c(4, 10, 16), each = 3),
           pch = 19, cex = 1.2)
    box()
    playedlist <- NULL
    i <- 0
    repeat {
        for (j in 1:2) {
            repeat {
                l <- locator(1)
                l$x <- min(n, max(1, round(l$x)))
                l$y <- min(n, max(1, round(l$y)))
                xy <- paste(l, collapse = ":")
                if (!is.element(xy, playedlist))
                    break
            }
            playedlist <- c(playedlist, xy)
            points(l, cex = 3, pch = c(19, 21)[j], bg = c("black", "white")[j])
            i <- i + 1
            if (i >= n^2) break
        }
        if (i >= n^2) break
    }
}
