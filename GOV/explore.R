library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(raster)
library(TSA)

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
        sen_of_center = sen_rep_in_sess - sen_dem_in_sess,
        hs_of_center = hs_rep_in_sess - hs_dem_in_sess
        ) %>%
    dplyr::filter(state != "Nebraska" & ## Data is no good for this state
                  year >= 1945 &
                  year <= 2011) 


pData <- ddply(pData, .(state), function(df){
    df <- df %>% dplyr::arrange(year)
    df <- col_diff(df, "sen_of_center")
    df <- col_diff(df, "hs_of_center")
}) %>%
    dplyr::mutate(sen_of_center_diff = sen_of_center_diff / sen_tot_in_sess,
                  hs_of_center_diff = hs_of_center_diff / hs_tot_in_sess)

ggplot(pData) +
    geom_line(aes(x = year, y = sen_of_center, group = state, color = state))

ggplot(pData) +
    geom_line(aes(x = year, y = hs_of_center, group = state, color = state))

ggplot(pData) +
    geom_line(aes(x = year, y = govparty_c, group = state, color = state))

ggplot(pData) +
    geom_line(aes(x = year, y = sen_elections_this_year,
                  group = state, color = state))

ggplot(pData) +
    geom_line(aes(x = year, y = hs_elections_this_year,
                  group = state, color = state))

ggplot(pData) +
    geom_line(aes(x = year, y = divided_gov,
                  group = state, color = state))

ggplot(pData) +
    geom_line(aes(x = year, y = sen_prop_up,
                  group = state, color = state))

ggplot(pData) +
    geom_line(aes(x = year, y = hs_prop_up,
                  group = state, color = state))



## just in CA
ggplot(pData %>% dplyr::filter(state == "California")) +
    geom_line(aes(x = year, y = sen_of_center, group = state, color = state))

## create a dataframe just with "sen_of_center" variable
cData <- dcast(pData, state ~ year, value.var = "sen_of_center")
cData <- cData[,!unlist(llply(cData, function(col){all(is.na(col))}))]

## look at distances between states' records
dists <- dist(cData[,-1])
heatmap(as.matrix(dists))


ret <- ccf(unlist(ca[,-c(i,1)]), nation[-i],
    type = "correlation")

## let's look at each state's acf:
ca <- pData %>%
    dplyr::arrange(year) %>%
    dplyr::filter(state == "California") %>%
    dplyr::filter(!is.na(sen_of_center))

tx <- pData %>%
    dplyr::arrange(year) %>%
    dplyr::filter(state == "Texas") %>%
    dplyr::filter(!is.na(sen_of_center))

ia <- pData %>%
    dplyr::arrange(year) %>%
    dplyr::filter(state == "Iowa") %>%
    dplyr::filter(!is.na(sen_of_center))


ggplot(ia, aes(x = year)) +
    ## geom_line(aes(y = hs_of_center), color = "red") +
    geom_line(aes(y = hs_of_center_diff), color = "red", linetype = "dotted") +
    ## geom_line(aes(y = sen_of_center), color = "blue") +
    geom_line(aes(y = sen_of_center_diff), color = "blue", linetype = "dotted") 

par(mfrow=c(2,1))

acf(na.omit(tx$hs_of_center_diff), main = "", ylab = "")
acf(na.omit(tx$sen_of_center_diff), main = "", ylab = "")

acf(na.omit(ia$hs_of_center_diff), main = "", ylab = "")
acf(na.omit(ia$sen_of_center_diff), main = "", ylab = "")

acf(na.omit(ca$hs_of_center_diff), main = "", ylab = "")
acf(na.omit(ca$sen_of_center_diff), main = "", ylab = "")



## look at acf's of all states together:
acfData <- ddply(pData, .(state), function(dat){
    ts <- dat %>%
        dplyr::arrange(year)
    sen_acf <- acf(na.omit(ts$sen_of_center_diff), plot = FALSE)
    hs_acf <- acf(na.omit(ts$hs_of_center_diff), plot = FALSE)
    sen_hs_ccf <- ccf(na.omit(ts$hs_of_center_diff),
                      na.omit(ts$sen_of_center_diff),
                      plot = FALSE)
    types <- rep(c("sen_acf", "hs_acf", "sen_hs_ccf"),
                 c(length(sen_acf$lag),
                   length(hs_acf$lag),
                   length(sen_hs_ccf$lag)))
    data.frame(state = unique(dat$state),
               type =  types,
               lag = c(sen_acf$lag, hs_acf$lag, sen_hs_ccf$lag),
               acf = c(sen_acf$acf, hs_acf$acf, sen_hs_ccf$acf),
               sd_line = 1.96 / sqrt(length(na.omit(ts$hs_of_center_dif)))
               )
})

ggplot(acfData, aes(x = lag, y = acf, group = lag)) +
    geom_boxplot() +
    geom_hline(aes(yintercept = -min(sd_line)), linetype = "dashed",
               color = "blue") +
    geom_hline(aes(yintercept = min(sd_line)), linetype = "dashed",
               color = "blue") +
    facet_grid(~type, scales = "free")

## let's attache their significances:
acfData <- ddply(pData, .(state), function(dat){
    ts <- dat %>%
        dplyr::arrange(year)
    sen_acf <- acf(na.omit(ts$sen_of_center_diff), plot = FALSE)
    hs_acf <- acf(na.omit(ts$hs_of_center_diff), plot = FALSE)
    sen_hs_ccf <- ccf(na.omit(ts$hs_of_center_diff),
                      na.omit(ts$sen_of_center_diff),
                      plot = FALSE)
    types <- rep(c("sen_acf", "hs_acf", "sen_hs_ccf"),
                 c(length(sen_acf$lag),
                   length(hs_acf$lag),
                   length(sen_hs_ccf$lag)))
    sd2 <- 1.96 / sqrt(length(na.omit(ts$hs_of_center_dif)))
    acfs <- c(sen_acf$acf, hs_acf$acf, sen_hs_ccf$acf)
    sigs <- acfs > sd2 | acfs < -sd2
    data.frame(state = unique(dat$state),
               type =  types,
               lag = c(sen_acf$lag, hs_acf$lag, sen_hs_ccf$lag),
               acf = acfs,
               sig = sigs)
})

ggplot(acfData %>% dplyr::filter(sig == TRUE),
       aes(x = lag, y = acf, group = lag, color = state)) +
    geom_point() +
    facet_grid(~type, scales = "free")
