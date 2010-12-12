# 2D heart with a Chinese five-stars (on the National flag)
# Author: weiwei; modified by Yihui Xie <xie@yihui.name>
# original code in a Chinese forum: http://cos.name/cn/topic/16743
# this code was written to celebrate China's 60 anniversary
n = 30000
p = sort(runif(n, min = 0, max = pi))
p = sin(p) + rnorm(n)
par(mar = c(2, 0.1, 3, 0.1), mgp = c(0.5, 0, 0))
plot(sort(rnorm(n)), -p, col = "red", pch = ".",
    axes = FALSE, xlim = c(-3.6, 3.6), ylim = c(-4.4, 3.3), ylab = "",
    xlab = "China's 60th Anniversary", main = "Happy Birthday, China!")
# stars
p <- q <- NULL
rr <- c(0.68, rep(0.32, 4))
m <- c(-1.2511927, -0.7707085, -0.5538095, -0.6129638,
    -0.8293381)
n <- c(-0.079779159, 0.42312375, 0.05632304, -0.45719796,
    -0.8440031)
for (k in c(1:5)) {
    r = rr[k]
    for (i in 1:5) {
        p[2 * i - 1] = sin(pi * 18/90)/sin(pi * 54/90) * r *
            sin(pi * (i * 72 + 36)/90)
        q[2 * i - 1] = sin(pi * 18/90)/sin(pi * 54/90) * r *
            cos(pi * (i * 72 + 36)/90)
        p[2 * i] = sin(pi * 18/90)/sin(pi * 54/90) * 0.38 * r *
            sin(pi * (i * 72 + 18)/90)
        q[2 * i] = sin(pi * 18/90)/sin(pi * 54/90) * 0.38 * r *
            cos(pi * (i * 72 + 18)/90)
    }
    x = p[c(1, 2, 5, 6, 9, 10, 3, 4, 7, 8, 1)]
    y = q[c(1, 2, 5, 6, 9, 10, 3, 4, 7, 8, 1)]
    x = x + m[k]
    y = y + n[k]
    polygon(x, y, col = "yellow", border = "yellow")
}
