##' function to find a UTM zone
##' \url{UTMzone}{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system#UTM_zone}
##' @title long2UTM 
##' @param long longitude
##' @return UTM Zone (integer) 
##' @author D G Rossiter
long2UTM <- function(long) {
  utmzone <- (floor((long + 180)/6) + 1)%%60 #floor(long + 180)/6) + 1) %% 60
  return(utmzone)
}

##' Function to extract and format one rectangular window
##'
##' @title extract hwsd data from a region
##' @param bbox a ❵raster✬-style extent argument, i.e., a vector of xmin, xmax, ymin, ymax
##' @param plot logical, return plot? (default FALSE)
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @examples
##' lat <- 44; lon <- -80; gridsize <- 0.1
##' bbox<-c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
##' extract.one(bbox)
extract.one <- function(bbox, plot = FALSE) {
  
  data(hwsd)
  hwsd.win <- crop(hwsd, extent(bbox))
  
  # find the zone for the centre of the box
  centre <- (bbox[1] + bbox[2])/2
  ##logger.info("Central meridian:", centre)
  utm.zone <- long2UTM(centre)
  ##logger.info("UTM zone:", utm.zone)
  # make a UTM version of the window
  hwsd.win.utm <- projectRaster(hwsd.win, 
                                crs = (paste("+proj=utm +zone=",
                                             utm.zone, "+datum=WGS84 +units=m +no_defs +ellps=WGS84
                                                       +towgs84=0,0,0",
                                             sep = "")), method = "ngb")
  ##logger.info("Cell dimensions:", paste((cell.dim <- res(hwsd.win.utm)), collapse = ", "))

  ## write the unprojected and projected raster images to disk
  ## writeRaster(hwsd.win, file = file.path(tempdir(), "hwsd.win"), overwrite=TRUE)
  ## writeRaster(hwsd.win.utm, file = file.path(tempdir(), "hwsd.win.utm"), overwrite=TRUE)
  
  ## extract attributes for just this window
  hwsd.sqlite <- system.file("extdata/HWSD.sqlite", package = "rhwsd")
  con <- dbConnect(dbDriver("SQLite"), dbname = "inst/extdata/HWSD.sqlite")
  
  dbWriteTable(con, name = "WINDOW_TMP", 
               value = data.frame(smu_id = unique(hwsd.win)),
               overwrite = TRUE)
  records <- dbGetQuery(con, "select T.* from HWSD_DATA as T join
                        WINDOW_TMP as U on T.mu_global=u.smu_id order by su_sym90")
  dbRemoveTable(con, "WINDOW_TMP")
  ## where appropriate, convert type to factor:
  factors <- c("MU_GLOBAL", "MU_SOURCE1", "MU_SOURCE2", "ISSOIL", "SU_SYM74", 
               "SU_CODE74", "SU_SYM85", "SU_CODE85", "SU_SYM90", "SU_CODE90", 
               "T_TEXTURE", "DRAINAGE", "AWC_CLASS", "PHASE1", "PHASE2", "T_USDA_TEX_CLASS", 
               "S_USDA_TEX_CLASS")
  for(f in factors) records[, f] <- as.factor(records[,f]) 
    
  ## make a spatial polygons dataframe, add attributes
  print(system.time(hwsd.win.poly <- rasterToPolygons(hwsd.win, n = 4,
                                                      na.rm = TRUE, dissolve = TRUE)))
  ## transform to UTM for correct geometry
  hwsd.win.poly.utm <- spTransform(hwsd.win.poly,
                                   CRS(proj4string(hwsd.win.utm)))
  m <- match(hwsd.win.poly.utm$value, records$MU_GLOBAL)
  hwsd.win.poly.utm@data <- records[m, ]
  ## plot the map unit ID
  lvls <-  length(levels(hwsd.win.poly.utm$MU_GLOBAL))
  ##logger.info("Number of legend categories in the map:", lvls)
  result <- list(records = records)
  if(plot){
    p1 <- spplot(hwsd.win.poly.utm, zcol = "MU_GLOBAL", col.regions =
                   terrain.colors(lvls),
                 main = paste("HWSD SMU code"), sub = paste("UTM zone", utm.zone),
                 scales = list(draw = TRUE))
    result <- append(result, list(records = records))
  }
  return(result)
  
}