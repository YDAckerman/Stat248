library(raster)
library(plyr)

shps <- list.files("districtShapes/", pattern = ".shp", full.names = TRUE)

dData <- ldply(shps, function(x){
    x <- shapefile(x)
    x$area_sqkm <- area(x) / 1000000
    data.frame(state = x$STATENAME,
               id = x$ID,
               district = x$DISTRICT,
               strt_cng = x$STARTCONG,
               end_cng = x$ENDCONG,
               area_sqkm = x$area_sqkm
               )
}, .progress = "text")

save(dData, file = "dData.rda")
