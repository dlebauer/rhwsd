##' Function to extract and format one rectangular window
##'
##' @title extract hwsd data from a region
##' @param x Either a vector specifying the corners of a box with \code{c(xmin, xmax, ymin, ymax)}; or a data frame with columns \code{lon} and \code{lat}.
##' @param con A connection to a data base returned by a function call \code{get_hwsd_con()}.
##' @param hwsd.bil (optional) location of file hwsd.bil. Too big for package repository. If necessary, this can be downloaded 
##' \url{here}{http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HWSD_Data/HWSD_RASTER.zip}
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @export
##' @examples
##' \dontrun{
##' lat <- 44; lon <- -80; gridsize <- 0.1
##' x <- c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
##' get_hwsd(x, con = con)
##' }
get_hwsd_siteset <- function(x, con = con, hwsd.bil = NULL){
  
  out <- purrr::map_dfr(
    as.list(seq(nrow(x))),
    ~get_hwsd(slice(x, .), con = con, hwsd.bil = hwsd.bil)
     ) %>% 
    right_join(x, by = c("lon", "lat"))
  return(out)
}

##' Function to extract and format one rectangular window
##'
##' @title extract hwsd data from a region
##' @param x Either a vector specifying the corners of a box with \code{c(xmin, xmax, ymin, ymax)}; or a data frame with columns \code{lon} and \code{lat}.
##' @param con A connection to a data base returned by a function call \code{get_hwsd_con()}.
##' @param hwsd.bil (optional) location of file hwsd.bil. Too big for package repository. If necessary, this can be downloaded 
##' \url{here}{http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HWSD_Data/HWSD_RASTER.zip}
##' @return records queried from region
##' @author D G Rossiter, David LeBauer
##' @export
##' @examples
##' \dontrun{
##' lat <- 44; lon <- -80; gridsize <- 0.1
##' x <- c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
##' get_hwsd(x, con = con)
##' }
get_hwsd <- function(x, con = con, hwsd.bil = NULL){
  
  ## read in raster object
  hwsd <- get_hwsd_raster(hwsd.bil = hwsd.bil)
  
  if ("data.frame" %in% class(x)){
    ## x is a data frame with 'lon' and 'lat' as columns
    ## interpret rows of the data frame as points for which data is to be extracted
    vals <- raster::extract(
        hwsd, 
        sp::SpatialPoints(dplyr::select(x, lon, lat)), # , proj4string = rasta@crs
        sp = TRUE) %>% 
      as_tibble() %>% 
      dplyr::pull(hwsd)
    
  } else {
    ## x is a vector c(xmin, xmax, ymin, ymax) 
    if(is.null(names(x))) names(x) <- c("lon", "lon", "lat", "lat")
    vals <- crop(hwsd, extent(x)) %>% 
      raster::unique()
  }

  ## extract attributes for just this window
  dbWriteTable(con, 
               name = "WINDOW_TMP", 
               value = data.frame(smu_id = vals),
               overwrite = TRUE
               )
  result <- dbGetQuery(con, "select T.* from HWSD_DATA as T join
                        WINDOW_TMP as U on T.mu_global=u.smu_id order by su_sym90")
  dbRemoveTable(con, "WINDOW_TMP")
  
  if ("data.frame" %in% class(x)){
    df_out <- result %>% 
      as_tibble() %>% 
      mutate(lon = x$lon, lat = x$lat) %>% 
      group_by(lon, lat) %>% 
      tidyr::nest()
  } else {
    df_out <- result %>% 
      as_tibble()
  }
  return(df_out)
  
}

##' Function to create connection to HWSD.sqlite database
##'
##' copies database so that any changes are not saved in package 
##' @title get HWSD connection
##' @return connection to HWSD.sqlite database
##' @author David LeBauer
##' @export
get_hwsd_con <- function(){
  hwsd.sqlite <- system.file("extdata/HWSD.sqlite", package = "rhwsd")
  if (hwsd.sqlite == "") hwsd.sqlite <- system.file("inst/extdata/HWSD.sqlite", package = "rhwsd")  
  file.copy(hwsd.sqlite, tempdir())
  db <- file.path(tempdir(), "HWSD.sqlite")
  con <<- dbConnect(dbDriver("SQLite"), dbname = hwsd.sqlite)
  return(con)
}

##' Function to create connection to HWSD.sqlite database
##'
##' copies database so that any changes are not saved in package 
##' @title get HWSD con
##' @param hwsd.bil (optional) location of file hwsd.bil. Too big for package repository. If necessary, this can be downloaded 
##' \url{here}{http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HWSD_Data/HWSD_RASTER.zip}
##' @export
##' @return hwsd a RasterLayer object with WGS84 projection
get_hwsd_raster <- function(hwsd.bil = NULL, download = FALSE){
  if(is.null(hwsd.bil)){
    hwsd.bil <- system.file("extdata/hwsd.bil", package = "rhwsd")
  }
  if(!file.exists(hwsd.bil)){
    if(download){
      download.file(url = "http://file-server.igb.illinois.edu/~dlebauer/hwsd/hwsd.zip",
                    dest.file = tempfile())
      data.dir <- system.file("extdata", package = "rhwsd")
      unzip(tempfile(), exdir = data.dir)
    } else {
      print("hwsd.is not available. It can be downloaded by setting the 'download = TRUE'\n")
      print("i.e.: get_hwsd_raster(download=TRUE)")
      print("if you already have the file, set hwsd.bil = '/path/to/hwsd.bil'")
    }

  }
  hwsd <- raster(hwsd.bil)
  proj4string(hwsd) <-  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  return(hwsd)
}