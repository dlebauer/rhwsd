\name{get.hwsd.raster}
\alias{get.hwsd.raster}
\title{get HWSD con}
\usage{
  get.hwsd.raster(hwsd.bil = NULL, download = FALSE)
}
\arguments{
  \item{hwsd.bil}{(optional) location of file hwsd.bil. Too
  big for package repository. If necessary, this can be
  downloaded
  \url{here}{http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HWSD_Data/HWSD_RASTER.zip}}
}
\value{
  hwsd a RasterLayer object with WGS84 projection
}
\description{
  Function to create connection to HWSD.sqlite database
}
\details{
  copies database so that any changes are not saved in
  package
}

