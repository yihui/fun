##' Sliding puzzle in R
##'
##' Use R to play sliding puzzle (currently only the Windows screen
##' display).
##'
##' If \code{size} is specified and \code{z} is \code{NULL}, then the
##' function will generate a solvable sliding puzzle. In addition, the
##' function only works under the Windows screen display because of
##' the limitation of function \code{getGraphicsEvent}.
##' @param size two dimensional vector, the size of sliding
##' puzzle. Note: the element of \code{size} must be greater than 1.
##' @param bg the background color of blocks.
##' @param z the matrix of sliding puzzle, if z is specified,
##' \code{size} will be omited.
##' @author Taiyun Wei
##' @note Linux/Mac users have to use \code{\link[grDevices]{X11}(type
##' = 'Xlib')} or the Cairo graphics device \code{Cairo()} in the
##' package \pkg{cairoDevice}.
##' @references About the sliding puzzle:
##' \url{http://en.wikipedia.org/wiki/Sliding_puzzle}
##'
##' How to Solve a Slider Puzzle:
##' \url{http://www.justadventure.com/articles/Slider/Slider.shtm}
##' @keywords iplot
##' @export
##' @examples
##' ## should use Xlib for the x11() device under *nix, e.g
##' \dontrun{if (.Platform$OS.type == 'windows') x11() else x11(type = 'Xlib')}
##'
##' sliding_puzzle()
##'
##' sliding_puzzle(z=matrix(0:11, 3, 4))
sliding_puzzle <- function(size = c(3, 3), bg = "lightblue", z = NULL) {
    if (!interactive()) return()
    if(!is.null(size)){
        n <- size[1]
        m <- size[2]
    }

    if(length(size)==1){
        n <- m <- size
    }

    if(!is.null(z)){
        n <- dim(z)[1]
        m <- dim(z)[2]
        if(!is.null(size))
            warning("Because \"z\" is specified, parameter \"size\" will be ignored.")
    }

    z.right <- matrix(1:(n*m), n, byrow = TRUE)
    z.right[n,m]<- 0

    ## calculate inverse number
    neg_seq.length <- function(x){
        len <- 0
        for(i in 1:(length(x) - 1)){
            tmp <- x[(i+1):length(x)] - x[i]
            len <- len + sum(tmp < 0)
        }
    }

    len.right <- neg_seq.length(as.vector(z.right)) + n + m


    if(is.null(z))
        z <- matrix(sample(0:(n*m - 1)), n)
    else {
        len.z <- neg_seq.length(as.vector(z)) + sum(which(z==0, arr.ind = TRUE))
        if((len.right%%2)!=(len.z%%2))
            stop("The sliding puzzle is insoluble!")
    }


    ## guarantee the game can be solved
    len.z <- neg_seq.length(as.vector(z)) + sum(which(z==0, arr.ind = TRUE))
    while((len.right%%2)!=(len.z%%2)| (all(z==z.right)) ){
    	z <- matrix(sample(0:(n*m - 1)), n)
    	len.z <- neg_seq.length(as.vector(z)) + sum(which(z==0, arr.ind = TRUE))
    }
    z[!z]<-NA

    ## plot puzzles
    replot <- function(z) {
        bg <- ifelse(z, bg, "white")
        fg <- ifelse(z, bg, "white")
        par(mar = c(0, 0, 0, 0), bg = "white")
        plot(c(0, m), c(0, n), type = "n",axes = FALSE, asp = 1, xlab = "",
             ylab = "")
        segments(0:m, rep(0, m + 1), 0:m, rep(n, m + 1), col = "grey",
                 lwd = 2)
        segments(rep(0, n + 1), 0:n, rep(m, n + 1), 0:n, col = "grey",
                 lwd = 2)
        symbols(0.5 + rep(0:(m - 1), each = n), 0.5 + rep((n -
                                     1):0, m), squares = rep(0.9, n*m), add = TRUE, inches = FALSE,
                fg = as.vector(fg), bg = as.vector(bg))
        text(0.5 + rep(0:(m - 1), each = n), 0.5 + rep((n -
                                  1):0, m), as.vector(z), cex = 3)
    }

    ##push function
    push <- function(x, begin, space) {
        tmp <- x[space]
        if (begin < space) {
            x[(begin + 1):space] <- x[begin:(space - 1)]
            x[begin] <- tmp
        }
        if (begin > space) {
            x[(begin - 1):space] <- x[begin:(space + 1)]
            x[begin] <- tmp
        }
        x
    }

    count <- 0
    mousedown <- function(buttons, x, y) {
        plx <- grconvertX(x, "ndc", "user")
        ply <- grconvertY(y, "ndc", "user")
        m.col <- ceiling(plx)
        m.row <- n - floor(ply)
        ind.NA <- which(is.na(z), arr.ind = TRUE)
        if (!xor(m.row == ind.NA[1], m.col == ind.NA[2]))
            cat("Warning: Cannot push any number!\n")

        ##row push
        ind.NA <- which(is.na(z), arr.ind = TRUE)
        if (ind.NA[1] == m.row & ind.NA[2] != m.col) {
            z[m.row, ] <<- push(z[m.row, ], m.col, ind.NA[2])
            cat("step = ", count <<- count + 1, "\n")
        }
        ##col push
        if (ind.NA[1] != m.row & ind.NA[2] == m.col) {
            z[, m.col] <<- push(z[, m.col], m.row, ind.NA[1])
            cat("step = ", count <<- count + 1, "\n")
        }
        replot(z)
        flag <- z == z.right
        if (all(flag[!is.na(flag)])){
            paste("You win! Time:", round((proc.time() - ptm)[3],2), "seconds.")
        }
    }

    ptm <- proc.time()
    replot(z)
    getGraphicsEvent("Game begin!", onMouseDown = mousedown)

}
