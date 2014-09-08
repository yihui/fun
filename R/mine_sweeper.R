#' Play the Mine Sweeper game in R
#'
#' The controls should be familiar to you: Click the left mouse button to dig in
#' an area, and right button to mark or unmark the area with flags.
#' @param width number of grids in horizontal axis
#' @param height number of grids in vertical axis
#' @param mines number of mines
#' @param cheat logical. If \code{TRUE} a matrix indicating the mines will be
#'   printed
#' @author Yixuan Qiu \email{yixuan.qiu@@cos.name}
#' @note Linux/Mac users have to use \code{X11(type = 'Xlib')} or the Cairo
#'   graphics device \code{Cairo()} in the package \pkg{cairoDevice}.
#' @references \url{http://en.wikipedia.org/wiki/Minesweeper_(computer_game)}
#' @export
#' @examples
#' ## should use Xlib for the x11() device under *nix, e.g
#' if (interactive()) {
#'   if (.Platform$OS.type == 'windows') x11() else x11(type = 'Xlib')
#'   mine_sweeper()
#' }
mine_sweeper <- function(width = 10, height = 10, mines = 20, cheat = FALSE) {
  # Deal with some exceptions
  if (!interactive()) return()
  if (mines >= width * height) {
    stop("Are you a terrorist??? Too many mines!")
  }
  if (width <= 0 | height <= 0 | mines <= 0) {
    stop("width, height and mines should be positive!")
  }
  width <- floor(width)
  height <- floor(height)
  mines <- floor(mines)

  m <- rep(0, width * height)
  # Status: 0 for untested areas, 1 for tested areas, 2 for flags
  mat.status <- matrix(m, height, width)
  mine.index <- sample(width * height, mines)
  m[mine.index] <- -10
  mine.mat <- matrix(m, height, width)
  search.mine <- which(mine.mat < 0, arr.ind = TRUE)
  mine.row <- search.mine[, 1]
  mine.col <- search.mine[, 2]
  # Calculate the number of mines in every 3x3 square
  for (i in 1:mines) {
    mrow <- intersect(1:height, (mine.row[i] - 1):(mine.row[i] + 1))
    mcol <- intersect(1:width, (mine.col[i] - 1):(mine.col[i] + 1))
    mine.mat[mrow, mcol] <- mine.mat[mrow, mcol] + 1
  }
  mine.mat <- ifelse(mine.mat < 0, -1, mine.mat)
  # -1 for mines
  if (cheat) print(mine.mat)

  # Plot a grid
  plot.grid <- function(x, y, w = 1, h = 1, col1 = "#D6E3F0", col2 = "#92B0CA", slices = 10) {
    # Generate contiguous colors
    f <- colorRampPalette(c(col1, col2))
    cols <- f(slices)
    xs <- rep(x, slices)
    ys <- seq(y + 0.5 * h - 0.5 * h / slices, y - 0.5 * h + 0.5 * h / slices,
              length.out = slices)
    gwidth <- rep(w, slices)
    gheight <- rep(h / slices, slices)
    # Rectangles with contiguous colors
    symbols(xs, ys, rectangles = cbind(gwidth, gheight), fg = cols, bg = cols,
            inches = FALSE, add = TRUE)
    #         polygon(x + c(-0.5, -0.5, -0.45) * w,
    #                 y + c(0.45, 0.5, 0.5) * h,
    #                 border = NA, col = "#DDDDDD")
    #         polygon(x + c(0.45, 0.5, 0.5) * w,
    #                 y + c(0.5, 0.5, 0.45) * h,
    #                 border = NA, col = "#DDDDDD")
    #         polygon(x + c(-0.5, -0.5, -0.45) * w,
    #                 y + c(-0.5, -0.45, -0.5) * h,
    #                 border = NA, col = "#DDDDDD")
    #         polygon(x + c(0.45, 0.5, 0.5) * w,
    #                 y + c(-0.5, -0.45, -0.5) * h,
    #                 border = NA, col = "#DDDDDD")
    #         polygon(x + c(-0.5, -0.45, 0.45, 0.5, 0.5, 0.45, -0.45, -0.5) * w,
    #                 y + c(0.45, 0.5, 0.5, 0.45, -0.45, -0.5, -0.5, -0.45) * h,
    #                 border = "#777777", lwd = 1)
    # Border
    polygon(x + c(-0.5, -0.5, 0.5, 0.5) * w, y + c(-0.5, 0.5, 0.5, -0.5) * h,
            border = "#777777")
  }

  # Plot the interface
  par(mar = c(0, 0, 0, 0), bg = "#DDDDDD")
  plot(1, type = "n", asp = 1, xlab = "", ylab = "",
       xlim = c(0.5, width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
  # Set font for X11 device
  if(.Device == "X11") {
    fixed <- X11Font("-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*")
    X11Fonts(fixed = fixed)
    par(family = "fixed")
  }
  x.grid <- (width + 1) / 2
  y.grid <- 1:height
  for (i in 1:height)  plot.grid(x.grid, y.grid[i], w = width, h = 1)
  x0 <- x1 <- seq(1.5, by = 1, length.out = width - 1)
  y0 <- rep(0.5, width - 1)
  y1 <- y0 + height
  segments(x0, y0, x1, y1, col = "#777777")

  # Colors to draw numbers
  col.palette <- c("DarkBlue", "ForestGreen", "brown", "green",
                   "blue", "yellow", "orange", "red")
  # Function to determine the font size of numbers
  text.cex <- function() {
    ps <- par("ps")
    0.6 * min(dev.size(units = "px") / c(width, height)) / ps
  }
  # Plot numbers -- vectorized
  plot.num <- function(x, y, num) {
    for(i in 1:length(x))  plot.grid(x[i], y[i], col1 = "#FFFFFF", col2 = "#C8C8C8")
    pnum = num[num > 0]
    px = x[num > 0]
    py = y[num > 0]
    text(px, py, pnum, col = col.palette[pnum], cex = text.cex())
  }
  # Draw unexploded mines -- vectorized
  plot.mine <- function(x, y) {
    for(i in 1:length(x))  plot.grid(x[i], y[i], col1 = "#FFFFFF", col2 = "#C8C8C8")
    symbols(x, y, circles = rep(0.35, length(x)),
            inches = FALSE, fg = NULL, bg = "black", add = TRUE)
    op = par(lend = 2)
    segments(x - 0.4, y, x + 0.4, y, col = "black", lwd = 5)
    segments(x, y - 0.4, x, y + 0.4, col = "black", lwd = 5)
    d = 0.4 / sqrt(2)
    segments(x - d, y - d, x + d, y + d, col = "black", lwd = 5)
    segments(x - d, y + d, x + d, y - d, col = "black", lwd = 5)
  }
  # Draw the exploded mine
  plot.mine.explode <- function(x, y) {
    plot.grid(x, y, col1 = "#FFFFFF", col2 = "#C8C8C8")
    star <- t(matrix(c(0.3, 0.4), 20, length(x)))
    symbols(x, y, stars = star, inches = FALSE, bg = "red", fg = NA, add = TRUE)
    symbols(x, y, stars = 0.7 * star, inches = FALSE, bg = "yellow", fg = NA, add = TRUE)
  }
  # Draw flags -- vectorized
  plot.flag <- function(x, y) {
    symbols(x + 0.075, y + 0.2,
            rectangles = matrix(rep(c(0.35, 0.2), rep(length(x), 2)), ncol = 2),
            inches = FALSE, fg = "red", bg = "red", add = TRUE)
    symbols(x, y - 0.25,
            rectangles = matrix(rep(c(0.6, 0.1), rep(length(x), 2)), ncol = 2),
            inches = FALSE, fg = "black", bg = "black", add = TRUE)
    segments(x - 0.1, y + 0.3, x - 0.1, y - 0.2)
  }
  search.zeroes <- function(pos, mat) {
    nr <- nrow(mat)
    nc <- ncol(mat)
    x <- ifelse(pos %% nr == 0, nr, pos %% nr)
    y <- ceiling(pos / nr)
    areas <- c(pos, (x > 1 & y > 1) * (pos - nr - 1), (y > 1) * (pos - nr),
               (x < nr & y > 1) * (pos - nr + 1), (x > 1) * (pos - 1),
               (x < nr) * (pos + 1), (x > 1 & y < nc) * (pos + nr - 1),
               (y < nc) * (pos + nr), (x < nr & y < nc) * (pos + nr + 1))
    areas <- unique(areas[areas != 0])
    zeroes <- intersect(areas, which(mat == 0))
    return(list(zeroes = zeroes, areas = areas))
  }

  mousedown <- function(buttons, x, y) {
    ## At least under Ubuntu, right click leads to buttons = c(0, 1)
    if (length(buttons) == 2) buttons <- 2
    plx <- round(grconvertX(x, "ndc", "user"))
    ply <- round(grconvertY(y, "ndc", "user"))
    ms <- mat.status
    if (plx < 1 || plx > width || ply < 1 || ply > height || buttons == 1) {
      return(ms)
    }
    current.status <- ms[height + 1 - ply, plx]
    current.mat <- mine.mat[height + 1 - ply, plx]
    ## Left button
    if (buttons == 0) {
      ## Untested area
      if (current.status == 0) {
        ## Is a mine
        if (current.mat == -1) {
          plot.mine(mine.col, height + 1 - mine.row)
          plot.mine.explode(plx, ply)
          cat("Game Over!\n")
          return(-1)
          ## Blank area
        } else if (current.mat == 0) {
          pos <- height * plx + 1 - ply
          while (TRUE) {
            temp <- pos
            lst <- search.zeroes(pos, mine.mat)
            pos <- lst$zeroes
            if (length(pos) == length(temp)) {
              areas <- lst$areas
              areas.row <- ifelse(areas %% height == 0, height, areas %% height)
              areas.col <- ceiling(areas / height)
              plot.num(areas.col, height + 1 - areas.row, mine.mat[areas])
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
          ## Numbered area
        } else {
          plot.num(plx, ply, current.mat)
          if (sum(ms == 1) == width * height - mines -1) {
            plot.flag(mine.col, height + 1 - mine.row)
            cat("You win!\n")
            return(1)
          }
          ms[height + 1 - ply, plx] <- 1
          return(ms)
        }
        ## Tested area or flag -- no action
      } else {
        return(ms)
      }
    }
    ## Right button
    if (buttons == 2) {
      ## Blank area
      if (current.status == 0) {
        ms[height + 1 - ply, plx] <- 2
        plot.flag(plx, ply)
        return(ms)
        ## Flag
      } else if (current.status == 2) {
        ms[height + 1 - ply, plx] <- 0
        plot.grid(plx, ply)
        return(ms)
        ## Numbered area -- no action
      } else {
        return(ms)
      }
    }
    return(ms)
  }

  while (TRUE) {
    if (length(mat.status) == 1) break
    mat.status <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
  }
}
