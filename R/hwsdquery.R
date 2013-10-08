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
<<<<<<< HEAD
##' extract.box(abox, con = con)
##' }
=======
##' extract.box(abox)
>>>>>>> 4da725334045e40c6fb98d80a2eb5abb3cb0f092
extract.box <- function(abox, con = con) {
  data(hwsd, package = "rhwsd")
  hwsd.win <- crop(hwsd, extent(abox))
  
  ## extract attributes for just this window
  dbWriteTable(con, name = "WINDOW_TMP", 
               value = data.frame(smu_id = raster::unique(hwsd.win)),overwrite = TRUE)
  result <- dbGetQuery(con, "select T.* from HWSD_DATA as T join
                        WINDOW_TMP as U on T.mu_global=u.smu_id order by su_sym90")
  dbRemoveTable(con, "WINDOW_TMP")
 
  return(result)
  
}

<<<<<<< HEAD
=======
##' Function to extract and format one rectangular window
##'
##' @title extract hwsd data from a region defined by a box
##' @param lat  degrees latitude
##' @param lon degrees longitude
##' @param gridsize size of bounding box in degrees 
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @export
myfun <- function(){
  
  a <- rasterFromXYZ(data.frame(a=1:2,b=2:3,c=3:4))
  b <- crop(a, extent(c(1,2,3,4)))
  raster::unique(a)
}

>>>>>>> 4da725334045e40c6fb98d80a2eb5abb3cb0f092

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
<<<<<<< HEAD
##' \dontrun{
##' extract.latlon(lat = 44, lon = -80, gridsize = 0.1, con = con)
##' }
=======
##' extract.latlon(lat = 44, lon = -80, gridsize = 0.1)
>>>>>>> 4da725334045e40c6fb98d80a2eb5abb3cb0f092
extract.latlon <- function(lat, lon, gridsize = 0.1, ...){
  abox<-c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
  result <- extract.box(abox, con = con)
  return(result)
}

##' Function to extract and format one rectangular window
##'
##' convenience wrapper for extract.latlon and extract.box
##' @title extract hwsd data from a region
##' @param abox (optional) 
##' @param lat (optional)
##' @param lon (optional)
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @export
##' @examples
<<<<<<< HEAD
##' \dontrun{
##' lat <- 44; lon <- -80; gridsize <- 0.1
##' abox<-c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
##' extract.one(abox)
##' }
=======
##' lat <- 44; lon <- -80; gridsize <- 0.1
##' abox<-c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
##' extract.one(abox)
>>>>>>> 4da725334045e40c6fb98d80a2eb5abb3cb0f092
extract.one <- function(...){
  ## http://stackoverflow.com/q/3057341/199217
  inputs <- list(...)
  input_list <- names(inputs)

  if(!"abox" %in% input_list){
    if(all(c("lat", "lon") %in% input_list)){
      lat <- inputs$lat
      lon <- inputs$lon
      if("gridsize" %in% input_list){
        abox <- get.box(lat = lat, lon = lon, gridsize = gridsize)      
      } else {
        abox <- get.box(lat = lat, lon = lon, gridsize = 0.1)
        print("no gridsize specified, using gridsize = 0.1")
      }
    } else {
      stop("must either specify box or lat, lon; see ?get.box")
    }
  } 
  result <- extract.box(abox)
  return(result)
}