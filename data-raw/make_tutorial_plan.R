## The plan the heat mitigation tour hands the user in place of an upload
## (inst/app/www/vft-tutorial-plan.png, see givePlan() in vft-tutorial.js): a
## generic square neighbourhood - buildings, roads, paths, trees, hedges, lawn
## and a pond - in flat fills the plan import (planimport.js) maps to materials
## by itself. Every fill is one of its REFERENCES colours or next to one, and
## covers well over its MIN_SHARE of 1 %, so none is dropped from the palette.
## No anti-aliasing: seven colours, a few kB.
##
## Run:  Rscript data-raw/make_tutorial_plan.R
out <- file.path("inst", "app", "www", "vft-tutorial-plan.png")
S <- 720

GRASS <- "#c8e6b0"; ROAD <- "#c0c0c0"; PATH <- "#d2b48c"; BUILDING <- "#404040"
TREE  <- "#2e6b34"; HEDGE <- "#6aa84f"; WATER <- "#87ceeb"

grDevices::png(out, width = S, height = S, type = "cairo", antialias = "none", bg = GRASS)
op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
graphics::plot.new()
graphics::plot.window(xlim = c(0, S), ylim = c(S, 0))   #y down, like the image

box   <- function(x0, y0, x1, y1, col) graphics::rect(x0, y0, x1, y1, col = col, border = NA)
line  <- function(x, y, col, lwd) graphics::lines(x, y, col = col, lwd = lwd, lend = "butt", ljoin = "round")
blob  <- function(x, y, r, col) graphics::symbols(x, y, circles = rep_len(r, length(x)), inches = FALSE,
                                                   add = TRUE, bg = col, fg = NA)
#1 px of lwd is 1/96 in; the device is 72 dpi
px <- function(w) w * 96 / 72

#ROADS: a cross, and a side street in the south-east
line(c(0, S), c(250, 250), ROAD, px(28))
line(c(430, 430), c(0, S), ROAD, px(28))
line(c(430, S), c(560, 560), ROAD, px(18))

#NORTH-WEST: two rows of housing blocks in lawn, hedges along the streets
for(x in c(30, 160, 290)) box(x, 28, x + 100, 78, BUILDING)
for(x in c(30, 160, 290)) box(x, 130, x + 100, 180, BUILDING)
box(14, 214, 400, 222, HEDGE)
box(402, 14, 410, 222, HEDGE)
blob(c(140, 270, 140, 270), c(104, 104, 200, 200), 11, TREE)

#NORTH-EAST: a hall and its yard
box(470, 30, 660, 140, BUILDING)
box(470, 160, 700, 224, ROAD)
blob(c(690, 690), c(60, 110), 12, TREE)

#SOUTH-WEST: a park - pond, gravel paths, groups of trees
th <- seq(0, 2 * pi, length.out = 120)
graphics::polygon(150 + 64 * cos(th), 470 + 42 * sin(th), col = WATER, border = NA)
line(c(0, 60, 150, 250, 340, 416), c(330, 380, 392, 370, 310, 290), PATH, px(10))
line(c(250, 270, 330, 360, 416), c(370, 480, 560, 640, 690), PATH, px(10))
line(c(60, 50, 90, 190, 270), c(380, 520, 600, 640, 720), PATH, px(10))
tx <- c(40, 70, 105, 30, 310, 345, 380, 300, 360, 395, 120, 160, 200, 140, 230, 290, 330,
        25, 60, 385, 400, 220, 180)
ty <- c(300, 290, 310, 345, 400, 390, 420, 440, 460, 500, 560, 545, 570, 600, 690, 680, 700,
        640, 690, 580, 620, 430, 700)
blob(tx, ty, 13 + (seq_along(tx) %% 3) * 2, TREE)

#ALONG THE MAIN STREETS: rows of trees
blob(seq(30, 390, 45), rep(276, 9), 8, TREE)
blob(rep(456, 5), seq(300, 520, 55), 8, TREE)

#SOUTH-EAST: small houses with gardens, a playground behind a hedge
for(y in c(290, 380, 470)) for(x in c(490, 600)) box(x, y, x + 70, y + 46, BUILDING)
box(476, 530, 706, 537, HEDGE)
box(480, 596, 590, 696, PATH)
box(610, 600, 700, 660, BUILDING)
box(476, 584, 706, 590, HEDGE)
blob(c(640, 690, 655), c(690, 695, 675), 10, TREE)
blob(c(580, 690, 580, 690), c(352, 352, 442, 442), 9, TREE)

graphics::par(op)
invisible(grDevices::dev.off())

img <- png::readPNG(out)
cols <- table(grDevices::rgb(img[, , 1], img[, , 2], img[, , 3]))
print(round(100 * sort(cols, decreasing = TRUE) / sum(cols), 1))
cat(sprintf("%s: %d x %d, %.1f kB, %d colours\n", out, dim(img)[2], dim(img)[1],
            file.size(out) / 1024, length(cols)))
