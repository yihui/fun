Rmine <- function(width = 10, height = 10, mines = 20, 
    text.cex = 2, cheat = FALSE) {
    if (mines >= width * height) {
        stop("Are you a terrorist???")
    }
    if (width <= 0 | height <= 0 | mines <= 0) {
        stop("Are you serious???")
    }
    width <<- width
    height <<- height
    mines <<- mines
    text.cex <<- text.cex
    m <- rep(0, width * height)
    mat.status <<- matrix(m, height, width)
    mine.index <- sample(1:(width * height), mines)
    m[mine.index] <- -10
    mine.mat <- matrix(m, height, width)
    mine.row <<- which(mine.mat < 0, arr.ind = TRUE)[, 1]
    mine.col <<- which(mine.mat < 0, arr.ind = TRUE)[, 2]
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
    x.grid <- outer(1:width, 1:height, function(x, y) x)
    y.grid <- outer(1:width, 1:height, function(x, y) y)
    x11(width, height)
    par(mar = c(0, 0, 0, 0))
    plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5, 
        width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
    plot.square <- function(x, y) {
        x.v <- c(x - 0.5, x - 0.5, x + 0.5, x + 0.5)
        y.v <- c(y - 0.5, y + 0.5, y + 0.5, y - 0.5)
        polygon(x.v, y.v, col = "white", border = "black")
    }
    for (i in 1:length(x.grid)) {
        plot.square(x.grid[i], y.grid[i])
    }
    mousedown <- function(buttons, x, y) {
        plot.mine <- function(x, y, color = "black") {
            r <- 0.3
            theta <- seq(0, 2 * pi, length.out = 100)
            c.x <- x - 0.1 + r * cos(theta)
            c.y <- y - 0.1 + r * sin(theta)
            polygon(c.x, c.y, col = color, border = color)
            lines(c(x, x + 0.2, x + 0.3), c(y, y + 0.2, y + 0.1), 
                col = color, lwd = 2)
        }
        plot.flag <- function(x, y) {
            polygon(c(x - 0.1, x - 0.1, x + 0.25, x + 0.25), 
                c(y + 0.1, y + 0.3, y + 0.3, y + 0.1), col = "red", 
                border = "red")
            polygon(c(x - 0.3, x - 0.3, x + 0.3, x + 0.3), c(y - 
                0.3, y - 0.2, y - 0.2, y - 0.3), col = "black", 
                border = "black")
            segments(x - 0.1, y + 0.3, x - 0.1, y - 0.2)
        }
        color <- c("grey", "DarkBlue", "forestgreen", "brown", 
            "green", "blue", "yellow", "orange", "red")
        plx <- round(grconvertX(x, "ndc", "user"))
        ply <- round(grconvertY(y, "ndc", "user"))
        ms <- mat.status
        if (plx < 1 | plx > width | ply < 1 | ply > height) {
            return(ms)
        }
        if (sum(ms == 1) == width * height - mines - 1 & mine.mat[height + 
            1 - ply, plx] != -1 & ms[height + 1 - ply, plx] == 
            0) {
            text(plx, ply, as.character(mine.mat[height + 1 - 
                ply, plx]), col = color[mine.mat[height + 1 - 
                ply, plx] + 1], cex = text.cex)
            for (i in 1:length(mine.index)) {
                plot.flag(mine.col[i], height + 1 - mine.row[i])
            }
            cat("You win!")
            return(1)
        }
        if (buttons == 0 & mine.mat[height + 1 - ply, plx] == 
            -1) {
            for (i in 1:height) {
                for (j in 1:width) {
                  if (mine.mat[i, j] == -1) {
                    plot.mine(j, height + 1 - i)
                  }
                  else {
                    text(j, height + 1 - i, as.character(mine.mat[i, 
                      j]), col = color[mine.mat[i, j] + 1], cex = text.cex)
                  }
                }
            }
            plot.mine(plx, ply, color = "red")
            cat("Game Over!")
            return(-1)
        }
        if (buttons == 0 & ms[height + 1 - ply, plx] == 0) {
            ms[height + 1 - ply, plx] <- 1
            text(plx, ply, as.character(mine.mat[height + 1 - 
                ply, plx]), col = color[mine.mat[height + 1 - 
                ply, plx] + 1], cex = text.cex)
            return(ms)
        }
        if (buttons == 2 & ms[height + 1 - ply, plx] == 0) {
            ms[height + 1 - ply, plx] <- 2
            plot.flag(plx, ply)
            return(ms)
        }
        if (buttons == 2 & ms[height + 1 - ply, plx] == 2) {
            ms[height + 1 - ply, plx] <- 0
            plot.square(plx, ply)
            return(ms)
        }
        return(ms)
    }
    while (1) {
        if (length(mat.status) == 1) 
            break
        mat.status <<- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
    }
} 
