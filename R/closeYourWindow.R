

##' Play the Close Your Window game in R
##' Use R to play the Close Your Window game (currently only for Windows
##' system).
##' 
##' In default, the white grids in the graphics stand for the windows that are
##' open, and black ones for the closed. When you click on a window, this
##' window as well as the four ones next to it will change its status. Your
##' mission is to close all the windows.
##' 
##' @param width number of grids in horizontal axis.
##' @param height number of grids in vertical axis.
##' @param steps number of "seed" grids to initialize the puzzle. In general,
##'   the larger \code{steps} is, the more complex this puzzle may be.
##' @param cheat logical. If \code{TRUE} a data frame indicating the steps to
##'   solve this puzzle will be printed.
##' @param col.closed color of closed window.
##' @param col.open color of open window.
##' @param col.frame color of window frame.
##' @param seed seed for random number generator.
##' @param \dots other arguments passed to \code{\link[base:Random]{set.seed}}.
##' @author Yixuan Qiu
##' @keywords iplot
##' @examples
##' 
##' \dontrun{
##' closeYourWindow(width=5, height=5, steps=3)
##' }
##' 
closeYourWindow <- function(width = 5, height = 5, 
    steps = 3, cheat = FALSE, col.closed = "black", col.open = "white", 
    col.frame = "lightblue", seed = NULL, ...) {
    assign("env", environment(), envir = .GlobalEnv)
    zmat <- mat.ini <- matrix(1, height, width)
    trans <- function(z, x, y) {
        nr <- nrow(z)
        nc <- ncol(z)
        mrow <- intersect(1:nr, (x - 1):(x + 1))
        mcol <- intersect(1:nc, (y - 1):(y + 1))
        z[x, y] <- z[x, y] * (-1)
        z[x, mcol] <- z[x, mcol] * (-1)
        z[mrow, y] <- z[mrow, y] * (-1)
        return(z)
    }
    if (!is.null(seed)) {
        set.seed(seed, ...)
    }
    grid.x <- sample(1:height, steps, replace = TRUE)
    grid.y <- sample(1:width, steps, replace = TRUE)
    if (cheat) {
        print(data.frame(row = grid.x, col = grid.y))
    }
    for (i in 1:steps) {
        zmat <- trans(zmat, grid.x[i], grid.y[i])
    }
    replot <- function(z) {
        nr <- nrow(z)
        nc <- ncol(z)
        xv <- rep(1:nc, rep(nr, nc))
        yv <- nr + +1 - rep(1:nr, nc)
        color <- ifelse(as.vector(z) == 1, col.closed, col.open)
        symbols(xv, yv, rectangles = matrix(1, length(xv), 2), 
            inches = FALSE, fg = col.frame, bg = color, add = TRUE)
    }
    x11(width, height)
    par(mar = c(0, 0, 0, 0))
    plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5, 
        width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
    replot(zmat)
    
    mousedown <- function(buttons, x, y) {
        zmat <- get("zmat", envir = env)
        nr <- nrow(zmat)
        nc <- ncol(zmat)
        plx <- round(grconvertX(x, "ndc", "user"))
        ply <- round(grconvertY(y, "ndc", "user"))
        if (plx < 1 | plx > nc | ply < 1 | ply > nr) {
            return(zmat)
        }
        zmat.trans <- trans(zmat, nr - ply + 1, plx)
        replot(zmat.trans)
        return(zmat.trans)
    }
    
    while (1) {
        if (!any(zmat == -1)) {
            cat("You win!")
            break
        }
        zmat <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
    }
}
