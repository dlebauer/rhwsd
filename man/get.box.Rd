\name{get.box}
\alias{get.box}
\title{get box}
\usage{
  get.box(lat, lon, gridsize = 0.1)
}
\arguments{
  \item{lat}{latitude}

  \item{lon}{longitude}

  \item{gridsize}{area around the point (size of Dx and
  Dy)}
}
\value{
  abox is a vecotr of xmin, xmax, ymin, ymax
}
\description{
  function to create a box
}
\details{
  distinct (and simpler) from \code{rgeos::get.box}
}
\author{
  David LeBauer
}

