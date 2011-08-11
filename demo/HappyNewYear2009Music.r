## "Happy New Year" music
## original code by Yixuan Qiu <yixuan.qiu@cos.name>
## URL: http://cos.name/cn/topic/13604
if (!require("tuneR"))
    stop("You need the \"tuneR\" package to play the music! (install.packages(\"tuneR\"))")
d = 20000
sil = sine(20, bit = 16, duration = 1000)
so.d = sine(392, bit = 16, duration = d)
si.d = sine(494, bit = 16, duration = d)
do = sine(523, bit = 16, duration = d)
re = sine(578, bit = 16, duration = d)
mi = sine(659, bit = 16, duration = d)
fa = sine(698, bit = 16, duration = d)
so = sine(784, bit = 16, duration = d)
music = bind(do, sil, do, sil, do, do, so.d, so.d, mi, sil,
mi, sil, mi, mi, do, do, sil, do, mi, so, so, sil, so,
so, fa, mi, re, re, re, re, sil, re, mi, fa, fa, sil,
fa, fa, mi, re, mi, mi, do, do, sil, do, mi, re, re,
so.d, so.d, si.d, re, do, do, do, do)
play(music)
