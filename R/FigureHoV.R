library(visualFields)
library(fields)
library(plotly)
library(data.table)

data("normvals")
data("locmaps")

load("myField.rda")

octopus_normal <- vf_table$get_norm_values(1) |> data.table()
sensitivities <- as.vector(unlist(octopus_normal[1, .SD, .SDcol = paste0("l", 1:59)]))
locations <- vf_table$locmaps[[1]] |> data.table()
locations <- locations[, .(x = xod, y = yod)]

grid.list <- list(
  x = seq(-100, 100, by = 1),  # ensure 0 is included
  y = seq(-100, 100, by = 1)
)

spline_model <- fields::Tps(locations, sensitivities, m = 2, df = 59)
tps <- fields::predictSurface(spline_model, grid.list = grid.list)

ymin <- 22

for (ix in seq(grid.list$x)) {
  for (iy in seq(grid.list$x)) {
    if (sqrt((tps$x[ix] - 15)^2 + (tps$y[iy] + 1.5)^2) < 3) {
      tps$z[ix, iy] <- ymin
    }
  }
}

pdf("hill_of_vision.pdf")
pmat <- persp(
  x = tps$x,
  y = tps$y,
  z = tps$z,
  xlab = "NASAL - TEMPORAL",
  xlim = c(-30, 30),
  ylab = "INFERIOR - SUPERIOR",
  ylim = c(-30, 30),
  zlab = "Light Sensitivity [dB]",
  zlim = c(ymin, 33),
  theta = 35,
  phi = 25,
  expand = .5,
  col = "red"
)

circx <- cos(seq(0, 2 * pi, length.out = 100))
circy <- sin(seq(0, 2 * pi, length.out = 100))

points(trans3d(0, 0, ymin, pmat))
lines(trans3d(3*circx + 15, 2*circy - 1.5, ymin, pmat))
lines(trans3d(10*circx, 10*circy, ymin, pmat))
lines(trans3d(20*circx, 20*circy, ymin, pmat))
lines(trans3d(30*circx, 30*circy, ymin, pmat))
lines(trans3d(c(0, 0), c(0, 0), c(ymin, max(sensitivities, na.rm = TRUE)), pmat))
lines(trans3d(c(-30, 30), c(0, 0), c(ymin, ymin), pmat))
lines(trans3d(c(0,0), c(-30, 30), c(ymin, ymin), pmat))
dev.off()
