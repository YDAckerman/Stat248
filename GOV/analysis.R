library(TSA)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(signal)

load("state_data.rda")
load("congress_data.rda")
load("regression_data.rda")

## goals:
## 1) autocovariances for all states
## 1.5) general distance measure
## 2) periodograms for all states
## 3) spectrograms
## 4) volatility analysis & model fitting

################################################################################
## Quick Notes
## It's going to be much easier to work with State House data. They have
## Elections every two years in which all seats are up for grabs. We'll show
## That House and Senate are nearly always quite correlated, but then we'll
## move to strictly looking at the houses

##############################
## plot ger-index to turnover
##############################
ger.data <- ldply(names(reg.data), function(state){
    df <- reg.data[[state]]
    data.frame(State = state,
               GI = df$Ger.Index[-1],
               res = diff(df$result),
               t = 1:(nrow(df) - 1))
})

ggplot(ger.data, aes(x = t, y = GI, group = State, color = State)) +
    geom_line() +
    xlab("Years since 1779") +
    ylab("Gerrymandering Index")

ggplot(ger.data, aes(x = GI, y = res)) +
    geom_point(aes(color = t)) +
    ggtitle("Gerrymandering Index vs House Turnover") +
    xlab("Gerrymandering Index") +
    ylab("House Turnover (Left (-) / Right (+) of Center)") +
    scale_color_continuous(guide = guide_legend(title = "Time"))

####################
## autocovariance
####################

## let's look at a few of the time series first:
selectStates <- pData %>%
    dplyr::arrange(year) %>%
    dplyr::filter(state %in% c("California", "Illinois",
                               "North Carolina", "Iowa")) %>%
    dplyr::filter(!is.na(sen_of_center))
                  
ggplot(selectStates, aes(x = year)) +
    geom_line(aes(y = hs_of_center_diff),
              color = "red", linetype = "solid") +
    geom_line(aes(y = sen_of_center_diff),
              color = "blue", linetype = "solid") +
        facet_grid(~state)
## Note: the sizes of state legislatures has changed over time

## Note: the trends in the House and Senate of these states is 
## often quite aligned (check out coherence)

########################
## correlation boxplots
########################
acfData <- ddply(pData, .(state), function(dat){
    ts <- dat %>%
        dplyr::arrange(year) %>%
        dplyr::filter(hs_prop_up != 0)
    sen_acf <- acf(na.omit(diff(ts$sen_of_center)), plot = FALSE)
    hs_acf <- acf(na.omit(diff(ts$hs_of_center)), plot = FALSE)
    sen_hs_ccf <- ccf(na.omit(diff(ts$hs_of_center)),
                      na.omit(diff(ts$sen_of_center)),
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

## this is interesting: nearly half of the State Houses have a significant
## autocorrelation at lag 1 (two years). No such "consensus" exists among
## State senates (likely because number of seats in each election changes).
## As we would have guessed, there is a strong positive cross-correlation
## between the Houses and Senates at lag 0. (Note: dashed line is the min
## over all sd-lines. Perhaps it should be the max - probably not too
## important right now)

########################
## Frequency Domain
########################

df <- pData %>%
    dplyr::filter(state %in% c("Rhode Island", "Nevada", "Arkansas",
                               "Alaska", "Nevada", "Montana",
                               "California", "Illinois", "North Carolina",
                               "Iowa"))

pgrams <- ddply(df, .(state), function(dat){
    pdg.dat <- periodogram(y = diff(na.omit(dat$hs_of_center)), plot = FALSE)
    data.frame(state = unique(dat$state),
               freq = pdg.dat$freq,
               spec = pdg.dat$spec)
})

ggplot(pgrams, aes(x = freq, y = spec)) +
    geom_line() +
    facet_wrap(~state)

pdg.dat <- periodogram(y = diff(na.omit(df$hs_of_center)), plot = FALSE)
ggplot(df, aes(x = year, y = hs_of_center_diff)) +
    geom_line() +
    facet_wrap(~state)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Connecticut"]]$result)))
specgram(diff(reg.data[["Connecticut"]]$result), n = 10, overlap = 9, Fs = 1)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Alaska"]]$result)))
specgram(diff(reg.data[["Alaska"]]$result), n = 10, overlap = 9, Fs = 1)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Nevada"]]$result)))
specgram(diff(reg.data[["Nevada"]]$result), n = 10, overlap = 9, Fs = 1)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Nevada"]]$result)))
specgram(diff(reg.data[["Nevada"]]$result), n = 10, overlap = 9, Fs = 1)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Montana"]]$result)))
specgram(diff(reg.data[["Montana"]]$result), n = 10, overlap = 9, Fs = 1)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["California"]]$result)))
specgram(diff(reg.data[["California"]]$result), n = 10, overlap = 9, Fs = 1)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Iowa"]]$result)))
specgram(diff(reg.data[["Iowa"]]$result), n = 10, overlap = 9, Fs = 1)


par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Illinois"]]$result)))
specgram(diff(reg.data[["Illinois"]]$result), n = 10, overlap = 9, Fs = 1)

par(mfrow=c(1,2))
plot(ts(diff(reg.data[["Texas"]]$result)))
tmp <- specgram(diff(reg.data[["Texas"]]$result), n = 10, overlap = 9, Fs = 1)


########################################
##  spectral mds
########################################
spec.data <- ldply(names(reg.data), function(state){
    spgrm <- specgram(diff(reg.data[[state]]$result), n = 10,
                         overlap = 3, Fs = 1)
    if(length(spgrm$t) != 4){
        tmp <- data.frame(State = state, t = NA, as.list(rep(NA, 4)))
        colnames(tmp) <- c("State", "t", paste0("freq", 1:4))
        tmp
    } else {
        spec.data <- ldply(1:length(spgrm$t), function(t){
        spec <- Mod(spgrm$S[,t])
        time <- spgrm$t[t]
        tmp <- data.frame(State = state, t = time, as.list(spec))
        colnames(tmp) <- c("State", "t", paste0("freq", 1:length(spec)))
        tmp
    })
    }
}, .inform = TRUE)

spec.data <- left_join(spec.data, ger.data, by = c("State", "t"))

spec.data <- spec.data[complete.cases(spec.data),]

spec.dist <- dist(spec.data[,paste0("freq", 1:5)])

fit <- cmdscale(spec.dist, eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]

spec.mds <- data.frame(State = spec.data$State, GI = spec.data$GI, X = x, Y = y)

ggplot(spec.mds, aes(x = X, y = Y, color = State, size = GI)) +
   geom_point() +
   theme(legend.position="none")

############################################################
## old but might use later: spectral boxplots
############################################################

## pdg.data <- ddply(pData, .(state), function(df){
##     pdg.dat <- periodogram(y = diff(na.omit(df$hs_of_center)), plot = FALSE)
##     data.frame(state = unique(df$state), freq = pdg.dat$freq,
##                spec = pdg.dat$spec)
## })

## ggplot(pdg.data, aes(x = freq, y = spec, group = freq)) +
##     geom_boxplot()

## ggplot(pdg.data, aes(x = freq, y = spec, color = state)) +
##     geom_point()

########################
## Probing for non iid
########################
acfData <- ddply(pData, .(state), function(dat){
    ts.hs <- dat %>%
        dplyr::arrange(year) %>%
        dplyr::filter(hs_prop_up != 0)
    hs_acf <- acf(abs(na.omit(diff(ts.hs$hs_of_center))), plot = FALSE)
    hs_pacf <- pacf(abs(na.omit(diff(ts.hs$hs_of_center))), plot = FALSE)
    types <- rep(c("hs_acf", "hs_pacf"),
                 c(length(hs_acf$lag),
                   length(hs_pacf$lag)))
    data.frame(state = unique(dat$state),
               type =  types,
               lag = c(hs_acf$lag, hs_pacf$lag),
               acf = c(hs_acf$acf, hs_pacf$acf),
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

####################
## fit ARCH model

## 1) fit AR models:
dat <- pData %>%
    dplyr::filter(state == "North Carolina" & hs_prop_up == 1)
hs.ts <- ts(na.omit(diff((dat %>% dplyr::arrange(year))$hs_of_center)))
ar.mod <- ar(hs.ts)
y <- na.omit(ar.mod$resid)
Box.test(y^2, lag = 2, type = "Ljung")
arch.y <- garch(y, order = c(1, 0))
summary(arch.y)

pd <- predict(arch.y)
plot(arch.y, type = "l")
lines(pd[,1], col = "blue", lty = "dashed")
lines(pd[,2], col = "blue", lty = "dashed")

####################
## Gen Dist
####################
## look at distances between states' records
cData <- pData %>%
    dplyr::mutate(perc_of_center = sen_of_center / sen_tot_in_sess)
cData <- dcast(cData, state ~ year, value.var = "perc_of_center")
cData <- cData[,unlist(llply(cData, function(col){all(is.na(col))}))]

dists <- dist(cData[,-1])
heatmap(as.matrix(dists))

##############################
## coherence
##############################

tx <- ger.data %>%
    dplyr::filter(.id == "Texas")

ca <- ger.data %>%
    dplyr::filter(.id == "California")

tmp <- cbind(tx$res, ca$res)
spc <- spectrum(tmp, spans = c(3,5))

tmp <- ger.data %>%
    dplyr::group_by(.id, GI) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(count == 10) 

ger.subset <- semi_join(ger.data, tmp, by = c(".id", "GI"))
                        
pairs <- combn(tmp$.id, 2)

coh.data <- ldply(1:ncol(pairs), function(i){
    pair <- pairs[,i]
    p1 <- ger.subset %>%
        dplyr::filter(.id == pair[1])
    p2 <- ger.subset %>%
        dplyr::filter(.id == pair[2])
    tmp <- cbind(p1$res, p2$res)
    spc <- spectrum(tmp, spans = c(3,5), plot = FALSE)
    data.frame(states = paste(pair, collapse = " - "),
               freq = spc$freq, coh = scale(spc$coh), ger1 = unique(p1$GI),
               ger2 = unique(p2$GI))
})

ggplot(coh.data, aes(x = freq, y = coh, group = states,
                     size = (ger1 - ger2)^2, color = states)) +
                         geom_smooth()

ggplot(coh.data %>%
         dplyr::filter(grepl("Rhode Island", states)),
       aes(x = freq, y = coh, group = states,
           size = (ger1 - ger2)^2, color = states)) +
               geom_smooth()

###################################
## Estimating a potential function
###################################

## Limit and clean the data

selectStates <- pData %>%
    dplyr::arrange(year) %>%
    dplyr::filter(state %in% c("California", "Illinois",
                               "North Carolina", "Iowa")) %>%
    dplyr::select(state, sen_of_center, hs_of_center,
                  year, election_year)

selectStates <- ddply(selectStates, .(state), function(sdf){
    sdf$diff_hs <- c(NA, diff(sdf$hs_of_center))
    sdf$diff_sen <- c(NA, diff(sdf$sen_of_center))
    sdf$dt <- c(NA, diff(sdf$year))
    sdf
})

## make sure it all looks right

ggplot(selectStates, aes(x = sen_of_center, y = hs_of_center)) +
    geom_line() +
    facet_wrap(~state)

ggplot(selectStates, aes(x = diff_sen, y = diff_hs)) +
    geom_line() +
    facet_wrap(~state)

## functions to help form the dataset
grad_potential <- function(x, y){
    row1 <- data.frame(1, 0, 2*x, y, 0, 3*x^2, x*y, y^2, 0)
    colnames(row1) <- paste0("x", 1:9)
    row2 <- data.frame(0, 1, 0, x, 2*y, 0, x^2, 2*x*y, 3*y^2)
    colnames(row2) <- paste0("x", 1:9)
    rbind(row1, row2)
}

potential <- function(x, y){
    tmp <- list(x, y, x^2, x*y, y^2, x^3, x^2 * y, x*y^2, y^3)
    do.call(cbind, tmp)
}

## form the dataset:
YX <- ldply(1:nrow(selectStates), function(i){
    row <- selectStates[i,]
    grad_row <- grad_potential(row$hs_of_center,
                               row$sen_of_center)
    grad_row$y <- c(row$diff_hs, row$diff_sen)
    grad_row$State <- row$state
    grad_row$dt <- row$dt
    grad_row
})

## run linear regression to fit potential
grid <- expand.grid(x = -100:100, y = -50:50)
p_vals <- potential(grid$x, grid$y)

reg <- ddply(YX, .(State), function(df){
    i <- is.na(df$y)
    X <- as.matrix(df[!i, paste0("x",1:9)])
    Y <- as.matrix(df[!i, "y"])
    beta <- solve(t(X) %*% X) %*% t(X) %*% Y
    p <- p_vals %*% beta
    grid$potential <- as.vector(p)
    grid
})

tmp <- reg %>% dplyr::filter(State == "Iowa")
ggplot(tmp, aes(x = x, y = y, fill = potential)) +
       geom_tile()
