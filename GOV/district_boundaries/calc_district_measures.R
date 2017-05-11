shps <- list.files("districtShapes/", pattern = ".shp", full.names = TRUE)

clean_shp <- function(shp){
    shp <- spTransform(shp, CRS(paste0("+proj=aea +lat_1=29.5 ",
                                     "+lat_2=45.5 +lat_0=37.5",
                                     " +lon_0=-96")))
    shp <- gSimplify(shp, tol = 0.000001)
    gBuffer(shp, byid=TRUE, width=0)
}

dData <- ldply(shps, function(shp){

    x <- shapefile(shp)
    shpData <- data.frame(state = x$STATENAME,
                          id = x$ID,
                          district = x$DISTRICT,
                          strt_cng = x$STARTCONG,
                          end_cng = x$ENDCONG,
                          perimeter = geosphere::perimeter(x))

    ## some of the polygons are a "self intersecting"
    x <- clean_shp(x)
    
    ## hull based measure
    g <- geom(x)
    ids <- unique(g[,1])

    x$hull <- sapply(ids, function(i){
           d <- g[g[,1] == i, ]
           area(polygons(convHull(d[, c('x', 'y')])))
    })
    x$area <- area(x)
    x$hrat <- x$hull / x$area

    ## perimeter based measure
    data.frame(shpData,
               area = x$area,
               hull = x$hull,
               hull_ratio = x$hrat
               )
    
}, .progress = "text")

save(dData, file = "dData.rda")
