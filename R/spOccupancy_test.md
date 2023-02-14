spOccupancy_test
================
Sunny Tseng
2023-02-11

## Library

``` r
library(here)
library(tidyverse)
library(spOccupancy)
library(coda)
library(stars)

set.seed(105)
```

## Import data

``` r
data(hbef2015)

sp.names <- dimnames(hbef2015$y)[[1]]
ovenHBEF <- hbef2015
ovenHBEF$y <- ovenHBEF$y[sp.names == "OVEN", , ]
table(ovenHBEF$y) # Quick summary.
```

    ## 
    ##   0   1 
    ## 518 588

Point count surveys were conducted at 373 sites over three replicates,
each of 10 minutes in length and with a detection radius of 100m. Some
sites were not visited for all three replicates. Additional information
on the data set (including individual species in the data set) can be
viewed in the man page using help(hbef2015).

hbef2015 is a list with four elements:

- y: a three-dimensional array of detection-nondetection data with
  dimensions of species (12), sites (373) and replicates (3).

- occ.covs: a numeric matrix with 373 rows and one column consisting of
  the elevation at each site.

- det.covs: a list of two numeric matrices with 373 rows and 3 columns.
  The first element is the day of year when the survey was conducted for
  a given site and replicate. The second element is the time of day when
  the survey was conducted.

- coords: a numeric matrix with 373 rows and two columns containing the
  site coordinates (Easting and Northing) in UTM Zone 19. The
  proj4string is “+proj=utm +zone=19 +units=m +datum=NAD83”.
