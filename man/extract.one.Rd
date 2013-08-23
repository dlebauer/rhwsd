\name{extract.one}
\alias{extract.one}
\title{extract hwsd data from a region}
\usage{
  extract.one(bbox, plot = FALSE)
}
\arguments{
  \item{bbox}{a ❵raster✬-style extent argument, i.e., a
  vector of xmin, xmax, ymin, ymax}

  \item{plot}{logical, return plot? (default FALSE)}
}
\value{
  records queried from region
}
\description{
  Function to extract and format one rectangular window
}
\examples{
lat <- 44; lon <- -80; gridsize <- 0.1
bbox<-c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
extract.one(bbox)
}
\author{
  D G Rossiter, David LeBauer
}

