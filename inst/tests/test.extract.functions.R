hwsd.sqlite <- system.file("extdata/HWSD.sqlite", package = "rhwsd")
if(hwsd.sqlite == "")hwsd.sqlite <- system.file("inst/extdata/HWSD.sqlite", package = "rhwsd")

con <<- dbConnect(dbDriver("SQLite"), dbname = hwsd.sqlite)

print(con)

expected.tables <- c("D_ADD_PROP", "D_AWC", "D_COVERAGE", "D_DRAINAGE", "D_IL", 
                     "D_ISSOIL", "D_PHASE", "D_ROOTS", "D_SWR", "D_SYMBOL", "D_SYMBOL74", 
                     "D_SYMBOL85", "D_SYMBOL90", "D_TEXTURE", "D_USDA_TEX_CLASS", 
                     "HWSD_DATA", "HWSD_SMU")
expected.colnames <- c("ID", "MU_GLOBAL", "MU_SOURCE1", "MU_SOURCE2", "ISSOIL", "SHARE", 
                       "SEQ", "SU_SYM74", "SU_CODE74", "SU_SYM85", "SU_CODE85", "SU_SYM90", 
                       "SU_CODE90", "T_TEXTURE", "DRAINAGE", "REF_DEPTH", "AWC_CLASS", 
                       "PHASE1", "PHASE2", "ROOTS", "IL", "SWR", "ADD_PROP", "T_GRAVEL", 
                       "T_SAND", "T_SILT", "T_CLAY", "T_USDA_TEX_CLASS", "T_REF_BULK_DENSITY", 
                       "T_BULK_DENSITY", "T_OC", "T_PH_H2O", "T_CEC_CLAY", "T_CEC_SOIL", 
                       "T_BS", "T_TEB", "T_CACO3", "T_CASO4", "T_ESP", "T_ECE", "S_GRAVEL", 
                       "S_SAND", "S_SILT", "S_CLAY", "S_USDA_TEX_CLASS", "S_REF_BULK_DENSITY", 
                       "S_BULK_DENSITY", "S_OC", "S_PH_H2O", "S_CEC_CLAY", "S_CEC_SOIL", 
                       "S_BS", "S_TEB", "S_CACO3", "S_CASO4", "S_ESP", "S_ECE")


test_that("database connection is valid",{
  dbtables <- dbListTables(con)
  expect_true(all(expected.tables %in% dbtables))
})

test_that("get.hwsd.latlon returns expected results",{
  ans <- get.hwsd.latlon(44, -88, 0.1, con = con)
  expect_true(all(expected.colnames %in% colnames(ans)))
  expect_equal(nrow(ans),  8)
})

test_that("get.hwsd.box returns expected results",{
  box <- c(-88.05, -87.95, 43.95, 44.05)
  ans <- get.hwsd.box(box, con = con)
  expect_true(all(expected.colnames %in% colnames(ans)))
  expect_equal(nrow(ans),  8)
})

test_that("get.hwsd.box and get.hwsd.latlon give equivalent answers",{
  ans1 <- get.hwsd.latlon(lat = 44, lon = -88, gridsize = 0.1, con = con)
  ans2 <- get.hwsd.box(c(-88.05, -87.95, 43.95, 44.05), con = con)
  expect_equal(ans1, ans2)
})

test_that("get.box returns box",{
  lat <- seq(-80, 80, length.out= 100)
  lon <- seq(-180, 180, length.out = 100)
  gridsize <- seq(0.1, 20, length.out = 100)
  
  set.seed(1)
  
  for(i in 1:10){
    lat <- sample(lat, 1)
    lon <- sample(lon, 1)
    gridsize <- sample(gridsize, 1)
    test <- get.box(lat, lon, gridsize)
    ## use "sample" to randomly select combinations
    expect_is(test, "Extent")
    testvec <- raster::as.vector(test)
    expect_equal(testvec[2] - testvec[1], gridsize)
    expect_equal(testvec[4] - testvec[3], gridsize)
  }
})

test_that("generic get.hwsd function works", {
  lat <- 44 
  lon <- -80 
  gridsize <- 0.1
  abox <- c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
  get.hwsd.box(abox, con = con)
  get.hwsd(lat=lat, lon = lon, gridsize=gridsize, con = con)
  get.hwsd(abox, con = con)
  
 })