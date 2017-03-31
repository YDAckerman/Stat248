library(raster)
library(plyr)
library(sp)
library(rgeos)

shps <- list.files("districtShapes/", pattern = ".shp", full.names = TRUE)

clean_shp <- function(shp){
    shp <- spTransform(shp, CRS(paste0("+proj=aea +lat_1=29.5 ",
                                     "+lat_2=45.5 +lat_0=37.5",
                                     " +lon_0=-96")))
    shp <- gSimplify(shp, tol = 0.000001)
    gBuffer(shp, byid=TRUE, width=0)
}

dData <- ldply(2:length(shps), function(i){

    x1 <- shapefile(shps[[i-1]])
    x2 <- shapefile(shps[[i]])
    x1 <- clean_shp(x1)
    x2 <- clean_shp(x2)

    intersections <- try(intersect(x1, x2))

    if(identical(class(intersections), "try-error")){
        x$change <- NA
    } else {
        x$change <- area(intersections) / 1000000
    }
    
    x$area_sqkm <- area(x) / 1000000
    x$ratio_change <- x$change / x$area_sqkm
    data.frame(state = x$STATENAME,
               id = x$ID,
               district = x$DISTRICT,
               strt_cng = x$STARTCONG,
               end_cng = x$ENDCONG,
               area_sqkm = x$area_sqkm,
               change_sqkm = x$change,
               ratio_change = x$ratio_change
               )
}, .progress = "text")

save(dData, file = "dData.rda")
