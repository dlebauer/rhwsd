
lat <- seq(-80, 80, length.out= 100)
lon <- seq(-180, 180, length.out = 100)
gridsize <- seq(0.1, 20, length.out = 100)
set.seed(1)

## use "sample" to randomly select combinations
test <- get.box(sample(lat),   sample(lon), sample(gridsize))

expect_true(all(test$xmin < test$xmax))
expect_true(all(test$ymin < test$ymax))
