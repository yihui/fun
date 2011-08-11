## 3D heart with a Chinese five-star flag
## Author: Yixuan Qiu <yixuan.qiu@cos.name>
## original code in a Chinese forum: http://cos.name/cn/topic/16743
## this code was written to celebrate China's 60 anniversary
if (!require("rgl")) stop("You need the rgl package to generate the 3D heart!")
xtheta = function(x, theta, y, w = 0, tt = 0) {
    (x^2 + (x * tan(theta))^2 + 2 * y^2 + 0.1 * cos(w * tt) -
     0.9)^3 - (x^2 + y^2/9) * (x * tan(theta))^3
}
fz = function(z, x, y, w = 0, tt = 0) {
    (x^2 + 2 * y^2 + z^2 + 0.1 * cos(w * tt) - 0.9)^3 - (x^2 + y^2/9) * z^3
}
n = 100
y = seq(-2, 2, length.out = n)
y0 = xx = zz = NULL
for (i in 1:length(y)) {
    theta = seq(-pi/2, 1.5 * pi, length.out = n)
    solvex = function(theta, y) {
        if (theta == -pi/2 | theta == pi/2 | theta == 1.5 * pi) {
            return(0)
        } else if (theta > -pi/2 & theta < pi/2) {
            interval = c(0, 2)
        } else {
            interval = c(-2, 0)
        }
        x.root = uniroot(xtheta, interval, theta, y)$root
        return(x.root)
    }
    if (xtheta(0, pi/4, y[i]) * xtheta(2, pi/4, y[i]) > 0)
        next
    y0 = c(y0, y[i])
    x = sapply(theta, solvex, y[i])
    zplus = uniroot(fz, c(0, 2), 0, y[i])$root
    zminus = uniroot(fz, c(-2, 0), 0, y[i])$root
    z = numeric(n)
    z[x != 0] = x[x != 0] * tan(theta[x != 0])
    z[x == 0] = (theta[x == 0] == pi/2) * zplus + (theta[x == 0] != pi/2) * zminus
    xx = cbind(xx, x)
    zz = cbind(zz, z)
}
yy = matrix(rep(y0, n), n, length(y0), byrow = TRUE)
library(rgl)
persp3d(zz, xx, yy, col = "red", xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1),
        axes = FALSE, box = FALSE, xlab = "", ylab = "", zlab = "")
fy = function(y, pars) {
    z = pars[1]
    x = pars[2]
    w = pars[3]
    tt = pars[4]
    (x^2 + 2 * y^2 + z^2 + 0.1 * cos(w * tt) - 0.9)^3 - (x^2 + y^2/9) * z^3
}
gety = function(z, x, interval = c(0.01, 1), w = 0, tt = 0) {
    mpars = cbind(z, x, w, tt)
    solvey = function(pars) {
        if (fy(interval[1], pars) * fy(interval[2], pars) > 0) {
            return(NA)
        } else {
            y = uniroot(fy, interval, pars)$root
        }
    }
    y = apply(mpars, 1, solvey)
    return(y)
}
x0 = z0 = seq(-1, 1, length.out = n)
y0 = outer(z0, x0, gety)
persp3d(x = z0, y = x0, z = y0, zlim = c(-1, 1), col = "white",
        texture = system.file("img", "flag.png", package = "fun"), add = TRUE)
persp3d(x = z0, y = x0, z = -y0, zlim = c(-1, 1), col = "red", add = TRUE)
