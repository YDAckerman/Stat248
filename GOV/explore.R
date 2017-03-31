library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(raster)

load("dataverse_files/all_data.rda")

## little helper function
col_diff <- function(df, col){
    df[, paste0(col, "_diff")] <- c(NA, diff(df[,col]))
    df
}

## look at change in legislative make up in all states
pData <- pData %>%
    dplyr::mutate(
        state = as.character(state),
        ofCenter = sen_rep_in_sess - sen_dem_in_sess 
        ) %>%
    dplyr::filter(state != "Nebraska") ## I've always hated the Cornhuskers.

pData <- ddply(pData, .(state), function(df){
    df <- df %>% dplyr::arrange(year)
    df <- col_diff(df, "ofCenter")
})

ggplot(pData) +
    geom_line(aes(x = year, y = ofCenter, group = state, color = state))

## I suspect the distribution from year to year is ~poisson, but that
## the mean decreases as year increases.
ggplot(pData, aes(x = abs(ofCenter_diff), group = year, color = year)) +
    geom_density()
## not very informative...


## just in CA
ggplot(pData %>% dplyr::filter(state == "California")) +
    geom_line(aes(x = year, y = ofCenter, group = state, color = state))

## look at the distribution of changes for a single year:
tmp <- pData %>%
    dplyr::filter(year == 2010) %>%
    dplyr::mutate(del = abs(del)) %>%
    dplyr::select(del)
qplot(tmp$del, geom = "density")

ggplot(pData, aes(x = year, y = abs(ofCenter_diff), group = year)) +
    geom_boxplot()

## create a dataframe just with "ofCenter" variable
cData <- dcast(pData, state ~ year, value.var = "ofCenter")
cData <- cData[,!unlist(llply(cData, function(col){all(is.na(col))}))]

nation <- colSums(cData[,-1], na.rm = TRUE)

qplot(x = as.numeric(colnames(cData)[-1]), y = nation, geom = "line")

qplot(1:(length(nation)-1), diff(nation), geom = "line")

plot(lm(diff(nation) ~ 1)$residuals) ## heteroskedasticity... gerrymandering?

## find ccf of each state's ts w/
testState <- cData %>% dplyr::filter(state == "California")
i <- which(is.na(testState))
ret <- ccf(unlist(testState[,-c(i,1)]), nation[-i],
    type = "correlation")


## I'm going to standardize each row of cData, then place all the
## rows end to end. After that I'm going to high-pass filter and
## fft - looking for frequencies.

## unlist(llply(1:nrow(cData), function(i){
##     row <- unlist(cData[i, -1])
##     max((seq_along(row))[is.na(row)])
## })) ## this tells us 1950 is a decent column for cut-off

## tmp <- ldply(1:nrow(cData), function(i){
##     row <- unlist(cData[i,16:77])
##     row <- rev(row)
##     row <- row[!is.na(row)][cumsum(!is.na(row))]
##     row <- rev(row)
##     data.frame(state = cData[i, "state"],
##                ofCenter = scale(row),
##                diff = c(NA, scale(diff(row))))
## })

## tmp$t <- 1:nrow(tmp)

## qplot(x = t, y = ofCenter, data = tmp, geom = "line")
## qplot(x = t, y = diff, data = tmp, geom = "line")

## ## from http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
## plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
##   plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
##   # TODO: why this scaling is necessary?
##   plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k), 2] 
##   plot(plot.data, t="h", lwd=2, main="", 
##        xlab="Frequency (Hz)", ylab="Strength", 
##        xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
## }

## X <- na.omit(ifelse(abs(tmp$diff) > 2, 1, 0))
## plot(X)

## plot.frequency.spectrum(fft())
