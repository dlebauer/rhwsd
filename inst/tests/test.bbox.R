
lat <- seq(-80, 80, length.out= 100)
lon <- seq(-180, 180, length.out = 100)
gridsize <- seq(0.1, 20, length.out = 100)
set.seed(1)

## use "sample" to randomly select combinations
test <- get.bbox(sample(lat),   sample(lon), sample(gridsize))

expect_true(all(bbox$xmin < bbox$xmax))
expect_true(all(bbox$ymin < bbox$ymax))