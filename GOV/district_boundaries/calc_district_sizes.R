library(raster)

args = commandArgs(trailingOnly=TRUE)

x <- shapefile(args[1])
crs(x)
x$area_sqkm <- area(x) / 1000000
