\name{extract.box}
\alias{extract.box}
\title{extract hwsd data from a region}
\usage{
  extract.box(abox, con = con)
}
\arguments{
  \item{abox}{a raster-style extent argument, i.e., a
  vector of xmin, xmax, ymin, ymax}
}
\value{
  records queried from region
}
\description{
  Function to extract and format one rectangular window
}
\examples{
<<<<<<< HEAD
\dontrun{
lat <- 44; lon <- -80; gridsize <- 0.1
abox <- c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
extract.box(abox, con = con)
}
=======
lat <- 44; lon <- -80; gridsize <- 0.1
abox <- c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
extract.box(abox)
>>>>>>> 4da725334045e40c6fb98d80a2eb5abb3cb0f092
}
\author{
  D G Rossiter, David LeBauer
}

