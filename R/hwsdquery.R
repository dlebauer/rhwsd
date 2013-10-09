##' function to find a UTM zone
##' \url{UTMzone}{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system#UTM_zone}
##' @title long2UTM 
##' @param long longitude
##' @return UTM Zone (integer)
##' @export
##' @author D G Rossiter
long2UTM <- function(long) {
  utmzone <- (floor((long + 180)/6) + 1)%%60 #floor(long + 180)/6) + 1) %% 60
  return(utmzone)
}

##' function to create a box
##'
##' distinct (and simpler) from \code{rgeos::get.box}
##' @title get box 
##' @param lat latitude
##' @param lon longitude
##' @param gridsize area around the point (size of Dx and Dy)
##' @return abox is a vecotr of xmin, xmax, ymin, ymax
##' @export
##' @author David LeBauer
get.box <- function(lat, lon, gridsize = 0.1){
  tmp <- c(lat, lat, lon, lon) + gridsize/2*c(-1,1,-1,1)# * c(sign(lati)*c(-1, 1), sign(loni)*c(-1, 1))
  abox <- extent(tmp)
  return(abox)
}

##' Function to extract and format one rectangular window
##'
##' @title extract hwsd data from a region
##' @param abox a raster-style extent argument, i.e., a vector of xmin, xmax, ymin, ymax
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @export
##' @examples
##' \dontrun{
##' lat <- 44; lon <- -80; gridsize <- 0.1
##' abox <- c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
##' get.hwsd.box(abox, con = con)
##' }
get.hwsd.box <- function(abox, con = con) {
  data(hwsd, package = "rhwsd")
  
  if(is.null(names(abox)))    names(abox) <- c("lon", "lon", "lat", "lat")
  hwsd.win <- crop(hwsd, extent(abox))

  ## extract attributes for just this window
  dbWriteTable(con, name = "WINDOW_TMP", 
               value = data.frame(smu_id = raster::unique(hwsd.win)),overwrite = TRUE)
  result <- dbGetQuery(con, "select T.* from HWSD_DATA as T join
                        WINDOW_TMP as U on T.mu_global=u.smu_id order by su_sym90")
  dbRemoveTable(con, "WINDOW_TMP")
 
  return(result)
  
}

##' Function to extract and format one rectangular window
##'
##' @title extract hwsd data from a region defined by a box
##' @param lat  degrees latitude
##' @param lon degrees longitude
##' @param gridsize size of bounding box in degrees 
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @export
##' @examples
##' \dontrun{
##' get.hwsd.latlon(lat = 44, lon = -80, gridsize = 0.1, con = con)
##' }
get.hwsd.latlon <- function(lat, lon, gridsize = 0.1, ...){
  abox <- c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
  result <- get.hwsd.box(abox, con = con)
  return(result)
}

##' Function to extract and format one rectangular window
##'
##' convenience wrapper for get.hwsd.latlon and get.hwsd.box
##' @title extract hwsd data from a region
##' @param abox (optional) 
##' @param lat (optional)
##' @param lon (optional)
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @export
##' @examples
##' \dontrun{
##' lat <- 44; lon <- -80; gridsize <- 0.1
##' abox<-c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
##' extract.one(abox)
##' }
get.hwsd <- function(...){
  ## http://stackoverflow.com/a/19259158/199217
  .args <- as.list(match.call())[-1]
  print(names(.args))
  if("abox" %in% names(.args)){
    hwsd.fn <- "get.hwsd.box"
  } else if (all(c("lat", "lon") %in% names(.args))) {
    hwsd.fn <- "get.hwsd.latlon"
  }
  print(hwsd.fn)
  result <- do.call(hwsd.fn, .args)
  return(result)
}