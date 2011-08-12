##' Play the Miner game in R
##'
##' The controls should be familiar to you: Click the left mouse
##' button to dig in an area, and right button to mark or unmark the
##' area with flags.
##' @param width number of grids in horizontal axis
##' @param height number of grids in vertical axis
##' @param mines number of mines
##' @param text.cex the amount by which text in graphics should be
##' magnified relative to the default. Adjust this parameter when the
##' size of text doesn't fit the grid
##' @param cheat logical. If \code{TRUE} a matrix indicating the mines
##' will be printed
##' @param seed seed for random number generator
##' @author Yixuan Qiu \email{yixuan.qiu@@cos.name}
##' @note Linux/Mac users have to use \code{X11(type = 'Xlib')} or the
##' Cairo graphics device \code{Cairo()} in the package
##' \pkg{cairoDevice}.
##' @references \url{http://en.wikipedia.org/wiki/Minesweeper_(computer_game)}
##' @keywords iplot
##' @export
##' @examples
##' ## should use Xlib for the x11() device under *nix, e.g
##' \dontrun{if (.Platform$OS.type == 'windows') x11() else x11(type = 'Xlib')}
##'
##' mine_sweeper()
mine_sweeper <- function(width = 10, height = 10, mines = 20,
    text.cex = 2, cheat = FALSE, seed = NULL) {
    if (!interactive()) return()
    if (mines >= width * height) {
        stop("Are you a terrorist???")
    }
    if (width <= 0 | height <= 0 | mines <= 0) {
        stop("Are you serious???")
    }
    m <- rep(0, width * height)
    mat.status <- matrix(m, height, width)
    if (!is.null(seed)) set.seed(seed)
    mine.index <- sample(1:(width * height), mines)
    m[mine.index] <- -10
    mine.mat <- matrix(m, height, width)
    mine.row <- which(mine.mat < 0, arr.ind = TRUE)[, 1]
    mine.col <- which(mine.mat < 0, arr.ind = TRUE)[, 2]
    for (i in 1:mines) {
        mrow <- intersect(1:height, (mine.row[i] - 1):(mine.row[i] +
            1))
        mcol <- intersect(1:width, (mine.col[i] - 1):(mine.col[i] +
            1))
        mine.mat[mrow, mcol] <- mine.mat[mrow, mcol] + 1
    }
    mine.mat <- ifelse(mine.mat < 0, -1, mine.mat)
    if (cheat) {
        print(mine.mat)
    }

    par(mar = c(0, 0, 0, 0))
    plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5,
        width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
    x.grid <- outer(1:width, 1:height, function(x, y) x)
    y.grid <- outer(1:width, 1:height, function(x, y) y)
    symbols(x.grid, y.grid, rectangles = matrix(1, length(x.grid),
        2), inches = FALSE, fg = "black", bg = "white", add = TRUE)

    plot.mine <- function(x, y, color = "black") {
        symbols(x - 0.1, y - 0.1, circles = rep(0.3, length(x)),
            inches = FALSE, fg = NULL, bg = color, add = TRUE)
        segments(x, y, x + 0.2, y + 0.2, col = color, lwd = 2)
        segments(x + 0.2, y + 0.2, x + 0.3, y + 0.1, col = color,
            lwd = 2)
    }
    plot.flag <- function(x, y) {
        symbols(x + 0.075, y + 0.2, rectangles = matrix(rep(c(0.35,
            0.2), rep(length(x), 2)), ncol = 2), inches = FALSE,
            fg = "red", bg = "red", add = TRUE)
        symbols(x, y - 0.25, rectangles = matrix(rep(c(0.6, 0.1),
            rep(length(x), 2)), ncol = 2), inches = FALSE, fg = "black",
            bg = "black", add = TRUE)
        segments(x - 0.1, y + 0.3, x - 0.1, y - 0.2)
    }
    search.zeroes = function(pos, mat) {
        nr <- nrow(mat)
        nc <- ncol(mat)
        x <- ifelse(pos%%nr == 0, nr, pos%%nr)
        y <- ceiling(pos/nr)
        areas <- c(pos, (x > 1 & y > 1) * (pos - nr - 1), (y >
            1) * (pos - nr), (x < nr & y > 1) * (pos - nr + 1),
            (x > 1) * (pos - 1), (x < nr) * (pos + 1), (x > 1 &
                y < nc) * (pos + nr - 1), (y < nc) * (pos + nr),
            (x < nr & y < nc) * (pos + nr + 1))
        areas <- unique(areas[areas != 0])
        zeroes <- intersect(areas, which(mat == 0))
        return(list(zeroes = zeroes, areas = areas))
    }
    mousedown <- function(buttons, x, y) {
        ## at least under Ubuntu, right click leads to buttons = c(0, 1)
        if (length(buttons) == 2) buttons <- 2
        color <- c("white", "grey", "DarkBlue", "ForestGreen",
            "brown", "green", "blue", "yellow", "orange", "red")
        plx <- round(grconvertX(x, "ndc", "user"))
        ply <- round(grconvertY(y, "ndc", "user"))
        ms <- mat.status

        current.status <- ms[height + 1 - ply, plx]
        current.mat <- mine.mat[height + 1 - ply, plx]
        if (plx < 1 || plx > width || ply < 1 || ply > height ||
            buttons == 1) {
            return(ms)
        }
        if (buttons == 0) {
            if (current.status == 0) {
                if (current.mat == -1) {
                  text(rep(1:width, rep(height, width)), height +
                    1 - rep(1:height, width), as.vector(mine.mat),
                    col = color[as.vector(mine.mat) + 2], cex = text.cex)
                  plot.mine(mine.col, height + 1 - mine.row,
                    color = "black")
                  plot.mine(plx, ply, color = "red")
                  cat("Game Over!\n")
                  return(-1)
                }
                else if (current.mat == 0) {
                  pos <- height * plx + 1 - ply
                  while (1) {
                    temp <- pos
                    lst <- search.zeroes(pos, mine.mat)
                    pos <- lst$zeroes
                    if (length(pos) == length(temp)) {
                      areas <- lst$areas
                      areas.row <- ifelse(areas%%height == 0,
                        height, areas%%height)
                      areas.col <- ceiling(areas/height)
                      text(areas.col, height + 1 - areas.row,
                        mine.mat[areas], col = color[mine.mat[areas] +
                          2], cex = text.cex)
                      ms[areas] <- 1
                      break
                    }
                  }
                  if (sum(ms == 1) == width * height - mines) {
                    plot.flag(mine.col, height + 1 - mine.row)
                    cat("You win!\n")
                    return(1)
                  }
                  return(ms)
                }
                else {
                  text(plx, ply, current.mat, col = color[current.mat +
                    2], cex = text.cex)
                  if (sum(ms == 1) == width * height - mines -
                    1) {
                    plot.flag(mine.col, height + 1 - mine.row)
                    cat("You win!\n")
                    return(1)
                  }
                  ms[height + 1 - ply, plx] <- 1
                  return(ms)
                }
            }
            else {
                return(ms)
            }
        }
        if (buttons == 2) {
            if (current.status == 0) {
                ms[height + 1 - ply, plx] <- 2
                plot.flag(plx, ply)
                return(ms)
            }
            else if (current.status == 2) {
                ms[height + 1 - ply, plx] <- 0
                symbols(plx, ply, rectangles = matrix(1, 1, 2),
                  inches = FALSE, fg = "black", bg = "white",
                  add = TRUE)
                return(ms)
            }
            else {
                return(ms)
            }
        }
        return(ms)
    }

    while (1) {
        if (length(mat.status) == 1)
            break
        mat.status <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
    }
}
