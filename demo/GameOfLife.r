## Game of Life (http://en.wikipedia.org/wiki/The_Game_of_Life)
## code by Linlin Yan <linlin.yan@cos.name>
## URL: http://cos.name/cn/topic/15402
row <- 100
col <- 100
init_life <- function(p) {
    m <- matrix(ifelse(runif(row * col) < p, 1, 0), row, col)
    m[1, ] = -1
    m[row, ] = -1
    m[, 1] = -1
    m[, col] = -1
    m
}
count_life <- function(m) {
    sum(ifelse(m < 0, 0, m))
}
next_life <- function(x) {
    t <- matrix(-1, row, col)
    for (i in 2:(row - 1)) {
        for (j in 2:(col - 1)) {
            c <- count_life(x[(i - 1):(i + 1), (j - 1):(j + 1)])
            t[i, j] <- ifelse((c >= 3) && (c <= 4), 1, 0)
        }
    }
    t
}
life_color <- c("#000000", "#004000", "#FFFF00")
draw_life <- function(m, generation) {
    image(m, col = life_color)
    title(paste("Generation ", generation))
}
generation <- 0
lives <- init_life(0.2)
while (TRUE) {
    draw_life(lives, generation)
    lives <- next_life(lives)
    generation <- generation + 1
}
