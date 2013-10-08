\name{extract.latlon}
\alias{extract.latlon}
\title{extract hwsd data from a region defined by a box}
\usage{
  extract.latlon(lat, lon, gridsize = 0.1, ...)
}
\arguments{
  \item{lat}{degrees latitude}

  \item{lon}{degrees longitude}

  \item{gridsize}{size of bounding box in degrees}
}
\value{
  records queried from region
}
\description{
  Function to extract and format one rectangular window
}
\examples{
\dontrun{
extract.latlon(lat = 44, lon = -80, gridsize = 0.1, con = con)
}
}
\author{
  D G Rossiter, David LeBauer
}

