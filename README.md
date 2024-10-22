# R interface to the Harmonized World Soil Database

**Archived**

See either the ingestr R package https://stineb.github.io/ingestr/ 
Or the netcdf version of the database https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247


This package was created long ago, when the database was originally released in MS Access format that was then converted to the sqlite database found in `inst/extdata`

---

This is a wrapper package containing the HWSD and helper functions. This package is based on:

## Code
Based on "Technical note: Processing the Harmonized World Soil Database (Version 1.2) in R" By D G Rossiter Dec 7, 2012 ([available online](http://www.itc.nl/~rossiter/teach/R/R_HWSD.pdf), with copy in the rwhsd package `man` folder).

## Data

2008-2009 COPYRIGHT FAO, IIASA, ISRIC, ISSCAS, JRC 
All rights reserved. No part of this Harmonized World Soil Database may be reproduced, stored in a retrieval system or transmitted by any means for resale or other commercial purposes without written permission of the copyright holders. Reproduction and dissemination of material in this information product for educational or other noncommercial purposes are authorized without any prior written permission from the copyright holders provided the source is fully acknowledged. Full acknowledgement and referencing of all sources must be included in any documentation using any of the material contained in the Harmonized World Soil Database, as follows:

## Citation 
FAO/IIASA/ISRIC/ISSCAS/JRC, 2009. Harmonized World Soil Database (version 1.1). FAO, Rome, Italy and IIASA, Laxenburg, Austria.
    
    
## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal: 
```{r}
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("stineb/rhwsd")
library(rhwsd)
```

### Dependencies

To install all required packages, do:
```{r}
list.of.packages <- c("dplyr", "purrr", "tidyr", "raster", "sp", "DBI", "RSQLite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```
    
## Usage

1. Download the HWSD data file [HWSD_RASTER.zip](http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HWSD_Data/HWSD_RASTER.zip) and extract.
2. Move the extracted files to a local directory (in this example: `"~/data/hwsd/HWSD_RASTER/hwsd.bil"`).

## Examples

Extract values for a rectangular box:
```r
library(rhwsd)
con <- get_hwsd_con()
ans <- get_hwsd(x = c(-88.5, -88, 44, 44.5), con = con, hwsd.bil = "~/data/hwsd/HWSD_RASTER/hwsd.bil")
```

Extract values for one point:
```r
con <- get_hwsd_con()
ans <- get_hwsd(x = data.frame(lon = -88, lat = 44), con = con, hwsd.bil = "~/data/hwsd/HWSD_RASTER/hwsd.bil")
```

Extract values for multiple points:
```r
con <- get_hwsd_con()
ans <- get_hwsd_siteset(x = data.frame(idx = 1:2, lon = c(-88, -88.5), lat = c(44, 44.5)), con = con, hwsd.bil = "~/data/hwsd/HWSD_RASTER/hwsd.bil")
```
