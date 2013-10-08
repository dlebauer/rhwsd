\name{extract.one}
\alias{extract.one}
\title{extract hwsd data from a region}
\usage{
  extract.one(...)
}
\arguments{
  \item{abox}{(optional)}

  \item{lat}{(optional)}

  \item{lon}{(optional)}
}
\value{
  records queried from region
}
\description{
  Function to extract and format one rectangular window
}
\details{
  convenience wrapper for extract.latlon and extract.box
}
\examples{
\dontrun{
lat <- 44; lon <- -80; gridsize <- 0.1
abox<-c(lon, lon, lat, lat) + gridsize/2 * c(-1, 1, -1, 1)
extract.one(abox)
}
}
\author{
  D G Rossiter, David LeBauer
}

