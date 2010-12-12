## A "Real" Turtle
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

draw_turtle <- function() {
    turtle_init()
    turtle_set_color("brown")
    turtle_forward(50)
    turtle_turn_left(120)
    turtle_forward(20)
    turtle_turn_left(30)
    turtle_forward(20)
    turtle_turn_left(30)
    turtle_forward(40)
    turtle_turn_left(30)
    turtle_forward(20)
    turtle_turn_left(30)
    turtle_forward(20)
    turtle_turn_left(120)
    turtle_forward(50)
    turtle_set_color("pink")
    turtle_pen_up()
    turtle_goto(-20, 0)
    turtle_pen_down()
    turtle_turn_right(120)
    turtle_forward(25)
    turtle_turn_right(60)
    turtle_forward(25)
    turtle_goto(-30, 0)
    turtle_turn_left(180)
    turtle_pen_up()
    turtle_goto(35, 0)
    turtle_pen_down()
    turtle_turn_right(90)
    turtle_forward(20)
    turtle_turn_right(60)
    turtle_forward(20)
    turtle_goto(25, 0)
    turtle_turn_left(150)
    turtle_pen_up()
    turtle_goto(50, 0)
    turtle_pen_down()
    turtle_turn_left(30)
    turtle_forward(30)
    turtle_turn_left(60)
    turtle_forward(10)
    turtle_turn_left(60)
    turtle_forward(10)
    turtle_turn_left(30)
    turtle_forward(10)
    turtle_goto(45, 10)
    turtle_pen_up()
    turtle_goto(-45, 0)
    turtle_pen_down()
    turtle_forward(17)
    turtle_turn_right(170)
    turtle_forward(19)
}
draw_turtle()
