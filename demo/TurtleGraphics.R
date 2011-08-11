## Turtle Graphics
## code by Linlin Yan <linlin.yan@cos.name>
## URL: http://cos.name/cn/topic/15876
turtle_x <- 0
turtle_y <- 0
turtle_direction <- 0
turtle_color <- "white"
turtle_drawing <- TRUE
turtle_init <- function(width = 100, height = 100,
    mar = rep(0, 4), bg = "black", ...) {
    par(mar = mar)
    par(bg = bg)
    plot(c(-width, width), c(-height, height), type = "n", xlab = "",
        ylab = "", axes = FALSE, ...)
    turtle_x <<- 0
    turtle_y <<- 0
    turtle_direction <<- 0
}
turtle_goto <- function(x, y) {
    if (turtle_drawing) {
        segments(turtle_x, turtle_y, x, y, col = turtle_color)
    }
    turtle_x <<- x
    turtle_y <<- y
}
turtle_pen_up <- function() {
    turtle_drawing <<- FALSE
}
turtle_pen_down <- function() {
    turtle_drawing <<- TRUE
}
turtle_forward <- function(distance) {
    x <- turtle_x + distance * cos(turtle_direction * pi/180)
    y <- turtle_y + distance * sin(turtle_direction * pi/180)
    turtle_goto(x, y)
}
turtle_turn_left <- function(degree) {
    turtle_direction <<- (turtle_direction + degree)%%360
}
turtle_turn_right <- function(degree) {
    turtle_direction <<- (turtle_direction - degree)%%360
}
turtle_set_color <- function(col) {
    turtle_color <<- col
}
turtle_demo <- function() {
    turtle_init()
    for (i in 1:4) {
        turtle_forward(50)
        turtle_turn_right(90)
    }
    readline("Press <Enter> to continue...")
    turtle_init()
    turtle_set_color("blue")
    for (i in 1:16) {
        for (j in 1:4) {
            turtle_forward(10)
            turtle_turn_right(90)
        }
        turtle_pen_up()
        turtle_forward(15)
        turtle_pen_down()
        if (i%%4 == 0) {
            turtle_turn_left(90)
        }
    }
    readline("Press <Enter> to continue...")
    turtle_init()
    turtle_set_color("gold")
    for (i in 1:5) {
        turtle_forward(100)
        turtle_turn_right(144)
    }
    readline("Press <Enter> to continue...")
    turtle_init()
    turtle_set_color("green")
    size <- 0.01
    for (i in 1:720) {
        turtle_forward(size)
        turtle_turn_right(3)
        size <- size * 1.01
    }
    readline("Press <Enter> to continue...")
    turtle_init()
    turtle_set_color("red")
    for (i in 1:20) {
        turtle_forward(50)
        turtle_turn_right(100)
    }
    readline("Press <Enter> to continue...")
    turtle_init()
    turtle_set_color("yellow")
    for (i in 1:36) {
        for (j in 1:4) {
            turtle_forward(5)
            turtle_turn_right(90)
        }
        turtle_turn_right(10)
        turtle_forward(15)
        for (j in 1:4) {
            turtle_forward(10)
            turtle_turn_right(90)
        }
        turtle_turn_right(10)
    }
    readline("Press <Enter> to continue...")
    turtle_init()
    for (i in 1:25) {
        turtle_set_color(rgb(10 * i, 5 * i, 255 - (10 * i), maxColorValue = 255))
        for (j in 1:9) {
            turtle_forward(100)
            turtle_turn_right(160)
        }
        turtle_turn_right(24)
    }
}
turtle_demo()
