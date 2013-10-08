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
  unique(a)
}