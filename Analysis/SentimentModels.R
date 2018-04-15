#################################################################
## File:            SentimentModels.R
##
## Description:     Merges stock data with sentiment scores,
##                  prepares data for modeling, and then performs
##                  various modeling algorithms.

## Housekeeping tasks
rm(list = ls())
gc()

require(randomForest)
require(xgboost)
require(caret)
require(miscTools)
require(ggplot2)
require(doParallel)
registerDoParallel(cores=4)
require(lattice)
require(TSPred)
require(lubridate)
require(dplyr)
require(plyr)
require(tseries)
require(timeSeries)

setwd("C:/Users/Owner/Documents/GitHub/MSDS_8390/SentimentStockPredictions_Expanded/Analysis")

########################################
###### Get stock price/statistics ######
########################################

## Get minute and hour data
getStockData <- function(tick, interval = "1"){
    #sink(paste0("getTrainTest_", tick, ".txt"))
    
    ## Get minute data and extract date, hour, and minute
    myKey <- readLines('Data/Keys/barchartAPI_key.txt')
    stock <- read.csv(paste0("https://marketdata.websol.barchart.com/getHistory.csv?apikey=", myKey, "&symbol=", tick, "&type=minutes&startDate=20180226&maxRecords=15000&interval=", interval, "&order=asc&sessionFilter=EFK&splits=true&dividends=true&volume=sum&nearby=1&jerq=true"), stringsAsFactors = FALSE)
    stock$timestamp <- ymd_hms(strptime(stock$timestamp,"%FT%H:%M:%S", tz = "EST"), tz = "EST")
    stock$date <- date(stock$timestamp)
    stock$hour <- hour(stock$timestamp)
    if(interval == "1"){
        stock$minute <- minute(stock$timestamp)
    }
    else if(interval == "60"){
        stock$close.diff <- c(NA,diff(stock$close))
        stock$return.percent <- returns(stock$close) * 100
    }
    
    return(stock)
}

AAPL.minute <- getStockData("AAPL", "1")
AAPL.hour <- getStockData("AAPL", "60")
XOM.minute <- getStockData("XOM", "1")
XOM.hour <- getStockData("XOM", "60")

## Calculate hourly volatility
addVolatility <- function(df.x, minuteData){
    #ddply usage inspired by https://stats.stackexchange.com/questions/7422/calculating-hourly-volatility-and-peak-to-average-ratio-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
    hourlyVol = ddply(.data = minuteData, .variables = .(date , hour),
                  .fun = function(x){
                      to_return = data.frame(volatility = sd(x$close))
                      return( to_return )
                  })
    df.merged <- merge(df.x, hourlyVol, by = c("date", "hour"), all.x = TRUE)
    df.merged <- df.merged[order(df.merged$date, df.merged$hour),]
    df.merged$volatility.diff <- c(NA, diff(df.merged$volatility)) #Create differenced volatility
    return(df.merged)
}

AAPL.response <- addVolatility(AAPL.hour, AAPL.minute)
XOM.response <- addVolatility(XOM.hour, XOM.minute)

## View basic plots of stock measures
plotResponses <- function(df, tick){
    par(mfrow=c(3,2))
    plot(df$close, type = 'l', main = paste(tick, "Hourly Close Price"))
    plot(df$close.diff, type = 'l', main = paste(tick, "Hourly Close Price (Differenced)"))
    plot(df$return.percent, type = 'l', main = paste(tick, "Hourly Return (%)"))
    plot(df$volatility, type = 'l', main = paste(tick, "Hourly Volatility"))
    plot(df$volatility.diff, type = 'l', main = paste(tick, "Hourly Volatility (Differenced)"))
    par(mfrow=c(1,1))
}

plotResponses(AAPL.response, "AAPL")
plotResponses(XOM.response, "XOM")

########################################
###### Merge Sentiment and Stocks ######
########################################

## Merge sentiment data with stock data for model development
mergeSentiment <- function(df, tick){
    tickSent <- readRDS(paste0('Data/TickerRDS/', tick, '.RDS'))
    
    ## Start with training data
    tickSent.train <- merge(tickSent, df[, c("date",
                                             "hour",
                                             "close",
                                             "close.diff",
                                             "return.percent",
                                             "volatility",
                                             "volatility.diff")],
                            by = c("date", "hour"), all.y = TRUE) #Keep only rows containing stock price (since sentiment was lagged before this, we still have lagged sentiment effects)
    tickSent.train <- tickSent.train[tickSent.train$date > "2018-02-26" & tickSent.train$date <= "2018-04-04",] #Incomplete sentiment data before 2-26-18
    tickSent.train[is.na(tickSent.train)] <- 0 #Fill any NA sentiment scores with 0
    colnames(tickSent.train) <- gsub(x = colnames(tickSent.train), pattern = "-", replacement = "N") #randomForest function doesn't like '-' in colnames
    
    ## Create prediction dataset next
    tickSent.pred <- merge(tickSent, df[, c("date",
                                            "hour",
                                            "close",
                                            "close.diff",
                                            "return.percent",
                                            "volatility",
                                            "volatility.diff")],
                           by = c("date", "hour"), all.y = TRUE) #Keep only rows containing stock price (since sentiment was lagged before this, we still have lagged sentiment effects)
    tickSent.pred <- tickSent.pred[tickSent.pred$date > "2018-04-04" & tickSent.pred$date <= "2018-04-13",] #20% of data set aside for predictions
    tickSent.pred[is.na(tickSent.pred)] <- 0 #Fill any NA sentiment scores with 0
    colnames(tickSent.pred) <- gsub(x = colnames(tickSent.pred), pattern = "-", replacement = "N") #randomForest function doesn't like '-' in colnames

    
    return(list(tickSent.train, tickSent.pred))
}

AAPL <- mergeSentiment(AAPL.response, "AAPL")
XOM <- mergeSentiment(XOM.response, "XOM")


##########################################
###### Check for Data Stationarity #######
##########################################

## Perform Dickey-Fuller and Augmented Dickey-Fuller tests
doDickey <- function(tickSent.train, tickSent.pred, tick){
    par(mfrow=c(2,1))
    #pdf(paste0('Stationarity_', tick, '.pdf'))
    temp <- apply(rbind(tickSent.train[2:length(tickSent.train)],tickSent.pred[2:length(tickSent.pred)]), 2, adf.test, alternative = "stationary", k=0)
    plot(sapply(temp, function(x) x$p.value), xlab = "Column Location", ylab = "Dickey-Fuller p.value", main = paste(tick, "Dickey-Fuller p.values"))

    temp <- apply(rbind(tickSent.train[2:length(tickSent.train)],tickSent.pred[2:length(tickSent.pred)]), 2, adf.test, alternative = "stationary")
    plot(sapply(temp, function(x) x$p.value), xlab = "Column Location", ylab = "Augmented Dickey-Fuller p.value", main = paste(tick, "Augmented Dickey-Fuller p.values"))
}

doDickey(AAPL[[1]], AAPL[[2]], "AAPL")
doDickey(XOM[[1]], XOM[[2]], "XOM")

###########################################
###### Generate Random Forest Model #######
###########################################
doRF <- function(train.df, pred.df, tick, response, formula, metric, xform = "", orig.train.df = train.df, orig.pred.df = pred.df, orig.response = response){
    tryCatch({
        ## Write outputs to external files for later review
        par(mfrow=c(1,1))
        sink(paste0("doRF_", tick, response, "_", metric, ".txt"))
        warningOut <- file(paste0("Warnings/doRF_", tick, response, "_", metric, "_warnings.txt"), open="wt")
        sink(warningOut, type = "message")
        pdf(paste0('RFplots_',tick, response, '_', metric, '.pdf'))

        ## Flow and plots inspired by and modified from http://blog.yhat.com/posts/comparing-random-forests-in-python-and-r.html
        ## Setup data
        cols <- colnames(train.df)
        cols <- cols[!cols %in% "date"]

        ## Create Random Forest Seeds
        # Seeding and timeslice methodology inspired by https://rpubs.com/crossxwill/time-series-cv
        set.seed(123)
        seeds <- vector(mode = "list", length = 79) #Length based on number of resamples + 1 for final model iteration
        for(i in 1:78) seeds[[i]] <- sample.int(1000, 72) #sample.int second argument value based on expand.grid length
        seeds[[79]] <- sample.int(1000, 1)

        ## Setup training parameters
        ts.control <- trainControl(method="timeslice", initialWindow = 98, horizon = 7, fixedWindow = FALSE, allowParallel = TRUE, seeds = seeds, search = "grid") #120 hour initial cv training, 35 hour cv testing
        tuneGridRF <- expand.grid(.mtry=c(1:72))
        #metric <- "Rsquared"

        ## Perform training
        start <- Sys.time() #Start timer
        rf <- train(formula, data = train.df[,cols], method = "rf", metric = metric, trControl = ts.control, tuneGrid = tuneGridRF, importance=TRUE)
        print(Sys.time() - start)
        #tuneGridRF <- expand.grid(.mtry=22)
        #rf <- train(high ~ ., data = train.df[,cols], method = "rf", metric = metric, trControl = ts.control, tuneGrid = tuneGridRF, importance=TRUE) #AAPL best mtry = 22
        cat("\nRF Output\n")
        print(rf)
        print(plot(rf))

        ## Evaluate metrics
        r2.train <- rSquared(train.df[,response], train.df[,response] - predict(rf, train.df[,cols]))
        r2.pred <- rSquared(pred.df[,response], pred.df[,response] - predict(rf, pred.df[,cols]))
        mse.train <- mean((train.df[,response] - predict(rf, train.df[,cols]))^2)
        mse.pred <- mean((pred.df[,response] - predict(rf, pred.df[,cols]))^2)
        rmse.train <- sqrt(mse.train)
        rmse.pred <- sqrt(mse.pred)
        mae.train <- mean(abs(train.df[,response] - predict(rf, train.df[,cols])))
        mae.pred <- mean(abs(pred.df[,response] - predict(rf, pred.df[,cols])))
        mape.train <- MAPE(train.df[,response], predict(rf, train.df[,cols]))
        mape.pred <- MAPE(pred.df[,response], predict(rf, pred.df[,cols]))

        ## Plot Rsquared Evaluation
        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=train.df[,response], pred=predict(rf, train.df[,cols])))
        print(p + geom_point() +
            geom_abline(color="red") +
            ggtitle(paste(tick, response, "RandomForest Regression: Training r^2 =", r2.train)))

        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=pred.df[,response], pred=predict(rf, pred.df[,cols])))
        print(p + geom_point() +
            geom_abline(color="red") +
            ggtitle(paste(tick, response, "RandomForest Regression: Prediction r^2 =", r2.pred)))

        if(xform == "diff"){
            ## Plot trained and predicted performance
            plot(as.numeric(c(cumsum(c(orig.train.df[,orig.response][1], train.df[,response])), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response])))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", main = paste0(tick, " RF Performance (", response, "): Training + Prediction"))
            axis(1, at=1:(sum(length(train.df[,response]), length(pred.df[,response]))), labels=FALSE)
            text(1:(sum(length(train.df[,response]), length(pred.df[,response]))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
            lines(c(cumsum(c(orig.train.df[,orig.response][1], predict(rf, train.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], predict(rf, pred.df[,cols])))), type = "l", lty = 2, lwd = 2, col = "red")
            abline(v = length(train.df[,response])+1, lty = 2, col = "blue")

            ## Plot just predicted performance
            plot(as.numeric(cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response]))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", ylim = c(min(c(cumsum(c(orig.pred.df[,orig.response][1], predict(rf, pred.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response])))), max(c(cumsum(c(orig.pred.df[,orig.response][1], predict(rf, pred.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response]))))), main = paste0(tick, " RF Performance (", response, "): Prediction"))
            axis(1, at=1:(length(pred.df[,response])), labels=FALSE)
            text(1:(length(pred.df[,response])), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
            lines(cumsum(c(orig.pred.df[,orig.response][1], predict(rf, pred.df[,cols]))), type = "l", lty = 2, lwd = 2, col = "red")
        }
        #else{
            ## Plot trained and predicted performance
            plot(as.numeric(c(train.df[,response], pred.df[,response])), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", main = paste0(tick, " RF Performance (", response, "): Training + Prediction"))
            axis(1, at=1:(sum(length(train.df[,response]), length(pred.df[,response]))), labels=FALSE)
            text(1:(sum(length(train.df[,response]), length(pred.df[,response]))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
            lines(c(predict(rf, train.df[,cols]),predict(rf, pred.df[,cols])), type = "l", lty = 2, lwd = 2, col = "red")
            abline(v = length(train.df[,response])+1, lty = 2, col = "blue")

            ## Plot just predicted performance
            plot(as.numeric(pred.df[,response]), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", ylim = c(min(c(predict(rf, pred.df[,cols]), pred.df[,response])), max(c(predict(rf, pred.df[,cols]), pred.df[,response]))), main = paste0(tick, " RF Performance (", response, "): Prediction"))
            axis(1, at=1:(length(pred.df[,response])), labels=FALSE)
            text(1:(length(pred.df[,response])), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
            lines(predict(rf, pred.df[,cols]), type = "l", lty = 2, lwd = 2, col = "red")
        #}

        ## Get feature importance
        feat.imp <- varImp(rf)
        plot(feat.imp, main = paste(tick, response, "RF Feature Importance"))

        on.exit(dev.off())
        on.exit(sink(type="message"), add = TRUE)
        on.exit(close(warningOut), add = TRUE)
        on.exit(sink(), add = TRUE)
    }, error = function(e){
        on.exit(dev.off())
        on.exit(sink(type="message"), add = TRUE)
        on.exit(close(warningOut), add = TRUE)
        on.exit(sink(), add = TRUE)
        print(paste(tick, response, "RF failed"))
    })

    return(list(rf, list(r2.train, r2.pred), list(mse.train, mse.pred), list(rmse.train, rmse.pred), list(mae.train, mae.pred), list(mape.train, mape.pred), feat.imp))
}

startOverall <- Sys.time() #Start Overall timer

## Predict 'close.diff'
setup_close.diff <- function(){
    notResponse <- c("close", "return.percent", "volatility", "volatility.diff")
    assign("AAPL.train", AAPL[[1]][,!(names(AAPL[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("AAPL.pred", AAPL[[2]][,!(names(AAPL[[2]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.train", XOM[[1]][,!(names(XOM[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.pred", XOM[[2]][,!(names(XOM[[2]]) %in% notResponse)], envir=.GlobalEnv)

    notResponse <- c("close.diff", "return.percent", "volatility", "volatility.diff")
    assign("AAPL.train.adder", AAPL[[1]][,!(names(AAPL[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("AAPL.pred.adder", AAPL[[2]][,!(names(AAPL[[2]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.train.adder", XOM[[1]][,!(names(XOM[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.pred.adder", XOM[[2]][,!(names(XOM[[2]]) %in% notResponse)], envir=.GlobalEnv)
}
setup_close.diff()

AAPL.closediff.rf.Rsquared <- doRF(AAPL.train, AAPL.pred, "AAPL", "close.diff", close.diff ~ ., "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.rf.Rsquared <- doRF(XOM.train, XOM.pred, "XOM", "close.diff", close.diff ~ ., "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "close")
AAPL.closediff.rf.RMSE <- doRF(AAPL.train, AAPL.pred, "AAPL", "close.diff", close.diff ~ ., "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.rf.RMSE <- doRF(XOM.train, XOM.pred, "XOM", "close.diff", close.diff ~ ., "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "close")

#Debug only
# train.df <- AAPL.train
# pred.df <- AAPL.pred
# tick <- "AAPL"
# response = "close.diff"
# orig.response = "close"
# formula = close.diff ~ .
# metric = "Rsquared"
# xform = "diff"
# orig.train.df <- AAPL.train.adder
# orig.pred.df <- AAPL.pred.adder


## Predict 'return.percent'
setup_return.percent <- function(){
    notResponse <- c("close", "close.diff", "volatility", "volatility.diff")
    assign("AAPL.train", AAPL[[1]][,!(names(AAPL[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("AAPL.pred", AAPL[[2]][,!(names(AAPL[[2]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.train", XOM[[1]][,!(names(XOM[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.pred", XOM[[2]][,!(names(XOM[[2]]) %in% notResponse)], envir=.GlobalEnv)
}
setup_return.percent()

AAPL.returnpercent.rf.Rsquared <- doRF(AAPL.train, AAPL.pred, "AAPL", "return.percent", return.percent ~ ., "Rsquared")
XOM.returnpercent.rf.Rsquared <- doRF(XOM.train, XOM.pred, "XOM", "return.percent", return.percent ~ ., "Rsquared")
AAPL.returnpercent.rf.RMSE <- doRF(AAPL.train, AAPL.pred, "AAPL", "return.percent", return.percent ~ ., "RMSE")
XOM.returnpercent.rf.RMSE <- doRF(XOM.train, XOM.pred, "XOM", "return.percent", return.percent ~ ., "RMSE")

## Predict 'volatility.diff'
setup_volatility.diff <- function(){
    notResponse <- c("close", "close.diff", "return.percent", "volatility")
    assign("AAPL.train", AAPL[[1]][,!(names(AAPL[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("AAPL.pred", AAPL[[2]][,!(names(AAPL[[2]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.train", XOM[[1]][,!(names(XOM[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.pred", XOM[[2]][,!(names(XOM[[2]]) %in% notResponse)], envir=.GlobalEnv)

    notResponse <- c("close", "close.diff", "return.percent", "volatility.diff")
    assign("AAPL.train.adder", AAPL[[1]][,!(names(AAPL[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("AAPL.pred.adder", AAPL[[2]][,!(names(AAPL[[2]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.train.adder", XOM[[1]][,!(names(XOM[[1]]) %in% notResponse)], envir=.GlobalEnv)
    assign("XOM.pred.adder", XOM[[2]][,!(names(XOM[[2]]) %in% notResponse)], envir=.GlobalEnv)
}
setup_volatility.diff()

AAPL.volatilitydiff.rf.Rsquared <- doRF(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", volatility.diff ~ ., "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.rf.Rsquared <- doRF(XOM.train, XOM.pred, "XOM", "volatility.diff", volatility.diff ~ ., "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "volatility")
AAPL.volatilitydiff.rf.RMSE <- doRF(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", volatility.diff ~ ., "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.rf.RMSE <- doRF(XOM.train, XOM.pred, "XOM", "volatility.diff", volatility.diff ~ ., "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "volatility")

print(Sys.time() - startOverall)

## Save models and metrics to external RDS
for(m in ls(pattern = '\\.rf\\.')){
    saveRDS(get(m), paste0('Data/ModelRDS/', m, '.RDS'))
}

###########################################
######### Generate XG Boost Model #########
###########################################
doXGB <- function(train.df, pred.df, tick, response, metric, xform = "", orig.train.df = train.df, orig.pred.df = pred.df, orig.response = response){
    tryCatch({
        ## Write outputs to external files for later review
        sink(paste0("doXGB_", tick, response, "_", metric, ".txt"))
        warningOut <- file(paste0("Warnings/doXGB_", tick, response, "_", metric, "_warnings.txt"), open="wt")
        sink(warningOut, type = "message")
        pdf(paste0('XGBplots_',tick, response, '_', metric, '.pdf'))

        ## Flow and plots inspired by and modified from http://blog.yhat.com/posts/comparing-random-forests-in-python-and-r.html
        ## Setup data
        cols <- colnames(train.df)
        cols <- cols[!cols %in% c("date", response)]
        X.train <- data.matrix(train.df[,cols])
        X.test <- data.matrix(pred.df[,cols])
        Y.train <- train.df[,response]
        Y.test <- pred.df[,response]

        ## Create seeds
        set.seed(123)
        seeds <- vector(mode = "list", length = 79) #Length based on number of resamples + 1 for final model iteration
        for(i in 1:78) seeds[[i]] <- sample.int(1000, 12) #sample.int second argument value based on expand.grid nrows
        seeds[[79]] <- sample.int(1000, 1)

        ## Setup training parameters
        ts.control <- trainControl(method="timeslice", initialWindow = 98, horizon = 7, fixedWindow = FALSE, allowParallel = TRUE, seeds = seeds, search = "grid") #35 day cv training, 14 day cv testing
        #metric <- "RMSE"
        tuneGridXGB <- expand.grid( #See parameter descriptions at http://xgboost.readthedocs.io/en/latest/parameter.html
            nrounds=350,
            eta = c(0.3, 0.5),
            gamma = c(0, 1, 5),
            max_depth = 6,
            colsample_bytree = c(0.5, 1),
            subsample = 0.5,
            min_child_weight = 1)

        ## Perform training
        start <- Sys.time() #Start timer
        xgbmod <- train(
            x = X.train,
            y = as.numeric(Y.train),
            method = 'xgbTree',
            metric = metric,
            trControl = ts.control,
            tuneGrid = tuneGridXGB,
            importance=TRUE)
        print(Sys.time() - start)
        cat("\nXGB Output\n")
        print(xgbmod)
        print(plot(xgbmod))

        ## Evaluate metrics
        r2.train <- rSquared(Y.train, Y.train - predict(xgbmod, X.train))
        r2.pred <- rSquared(Y.test, Y.test - predict(xgbmod, X.test))
        mse.train <- mean((Y.train - predict(xgbmod, X.train))^2)
        mse.pred <- mean((Y.test - predict(xgbmod, X.test))^2)
        rmse.train <- sqrt(mse.train)
        rmse.pred <- sqrt(mse.pred)
        mae.train <- mean(abs(Y.train - predict(xgbmod, X.train)))
        mae.pred <- mean(abs(Y.test - predict(xgbmod, X.test)))
        mape.train <- MAPE(Y.train, predict(xgbmod, X.train))
        mape.pred <- MAPE(Y.test, predict(xgbmod, X.test))

        ## Plot Rsquared Evaluation
        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=Y.train, pred=predict(xgbmod, X.train)))
        print(p + geom_point() +
            geom_abline(color="red") +
            ggtitle(paste(tick, response, "XGBoost Regression in R r^2 =", r2.train)))

        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=Y.test, pred=predict(xgbmod, X.test)))
        print(p + geom_point() +
            geom_abline(color="red") +
            ggtitle(paste(tick, response, "XGBoost Regression in R r^2 =", r2.pred)))

        if(xform == "diff"){
            ## Plot trained and predicted performance
            plot(as.numeric(c(cumsum(c(orig.train.df[,orig.response][1], Y.train)), cumsum(c(orig.pred.df[,orig.response][1], Y.test)))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", main = paste0(tick, " XGBoost Performance (", response, "): Training + Prediction"))
            axis(1, at=1:(sum(length(Y.train), length(Y.test))), labels=FALSE)
            text(1:(sum(length(Y.train), length(Y.test))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
            lines(c(cumsum(c(orig.train.df[,orig.response][1], predict(xgbmod, X.train))), cumsum(c(orig.pred.df[,orig.response][1], predict(xgbmod, X.test)))), type = "l", lty = 2, lwd = 2, col = "red")
            abline(v = length(Y.train)+1, lty = 2, col = "blue")

            ## Plot just predicted performance
            plot(as.numeric(cumsum(c(orig.pred.df[,orig.response][1], Y.test))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", ylim = c(min(c(cumsum(c(orig.pred.df[,orig.response][1], predict(xgbmod, X.test))), cumsum(c(orig.pred.df[,orig.response][1], Y.test)))), max(c(cumsum(c(orig.pred.df[,orig.response][1], predict(xgbmod, X.test))), cumsum(c(orig.pred.df[,orig.response][1], Y.test))))), main = paste0(tick, " XGBoost Performance (", response, "): Prediction"))
            axis(1, at=1:(length(Y.test)), labels=FALSE)
            text(1:(length(Y.test)), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
            lines(cumsum(c(orig.pred.df[,orig.response][1], predict(xgbmod, X.test))), type = "l", lty = 2, lwd = 2, col = "red")
        }
        #else{
            ## Plot trained and predicted performance
            plot(as.numeric(c(Y.train, Y.test)), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", main = paste0(tick, " XGBoost Performance (", response, "): Training + Prediction"))
            axis(1, at=1:(sum(length(Y.train), length(Y.test))), labels=FALSE)
            text(1:(sum(length(Y.train), length(Y.test))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
            lines(c(predict(xgbmod, train.df[,cols]),predict(xgbmod, X.test)), type = "l", lty = 2, lwd = 2, col = "red")
            abline(v = length(Y.train)+1, lty = 2, col = "blue")

            ## Plot just predicted performance
            plot(as.numeric(Y.test), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", ylim = c(min(c(predict(xgbmod, X.test), Y.test)), max(c(predict(xgbmod, X.test), Y.test))), main = paste0(tick, " XGBoost Performance (", response, "): Prediction"))
            axis(1, at=1:(length(Y.test)), labels=FALSE)
            text(1:(length(Y.test)), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
            lines(predict(xgbmod, X.test), type = "l", lty = 2, lwd = 2, col = "red")
        #}
        tryCatch({
            feat.imp <- varImp(xgbmod)
            print(plot(feat.imp, main = paste(tick, "XGBoost Feature Importance")))

            on.exit(dev.off())
            on.exit(sink(type="message"), add = TRUE)
            on.exit(close(warningOut), add = TRUE)
            on.exit(sink(), add = TRUE)
            return(list(xgbmod, list(r2.train, r2.pred), list(mse.train, mse.pred), list(rmse.train, rmse.pred), list(mae.train, mae.pred), list(mape.train, mape.pred), feat.imp))
        }, error = function(e){
            on.exit(dev.off())
            on.exit(sink(type="message"), add = TRUE)
            on.exit(close(warningOut), add = TRUE)
            on.exit(sink(), add = TRUE)
            return(list(xgbmod, list(r2.train, r2.pred), list(mse.train, mse.pred), list(rmse.train, rmse.pred), list(mae.train, mae.pred), list(mape.train, mape.pred), NA))
        })
    }, error = function(e){
        on.exit(dev.off())
        on.exit(sink(type="message"), add = TRUE)
        on.exit(close(warningOut), add = TRUE)
        on.exit(sink(), add = TRUE)
        print(paste(tick, response, "XGB failed"))
    })

}
 
startOverall <- Sys.time() #Start Overall timer

## Predict 'close.diff'
setup_close.diff()

AAPL.closediff.xgb.Rsquared <- doXGB(AAPL.train, AAPL.pred, "AAPL", "close.diff", "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.xgb.Rsquared <- doXGB(XOM.train, XOM.pred, "XOM", "close.diff", "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "close")
AAPL.closediff.xgb.RMSE <- doXGB(AAPL.train, AAPL.pred, "AAPL", "close.diff", "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.xgb.RMSE <- doXGB(XOM.train, XOM.pred, "XOM", "close.diff", "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "close")

## Predict 'return.percent'
setup_return.percent()

AAPL.returnpercent.xgb.Rsquared <- doXGB(AAPL.train, AAPL.pred, "AAPL", "return.percent", "Rsquared")
XOM.returnpercent.xgb.Rsquared <- doXGB(XOM.train, XOM.pred, "XOM", "return.percent", "Rsquared")
AAPL.returnpercent.xgb.RMSE <- doXGB(AAPL.train, AAPL.pred, "AAPL", "return.percent", "RMSE")
XOM.returnpercent.xgb.RMSE <- doXGB(XOM.train, XOM.pred, "XOM", "return.percent", "RMSE")

#Debug only
# train.df <- AAPL.train
# pred.df <- AAPL.pred
# tick <- "AAPL"
# response = "return.percent"
# orig.response = response
# metric = "Rsquared"
# xform = ""
# orig.train.df <- train.df
# orig.pred.df <- pred.df

## Predict 'volatility.diff'
setup_volatility.diff()

AAPL.volatilitydiff.xgb.Rsquared <- doXGB(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.xgb.Rsquared <- doXGB(XOM.train, XOM.pred, "XOM", "volatility.diff", "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "volatility")
AAPL.volatilitydiff.xgb.RMSE <- doXGB(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.xgb.RMSE <- doXGB(XOM.train, XOM.pred, "XOM", "volatility.diff", "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "volatility")

print(Sys.time() - startOverall)

## Save models and metrics to external RDS
for(m in ls(pattern = '\\.xgb\\.')){
    saveRDS(get(m), paste0('Data/ModelRDS/', m, '.RDS'))
}


####################################################
######### Generate KNN Regression Model ############
####################################################
doKNN <- function(train.df, pred.df, tick, response, formula, metric, xform = "", orig.train.df = train.df, orig.pred.df = pred.df, orig.response = response){
    tryCatch({
        ## Write outputs to external files for later review
        par(mfrow=c(1,1))
        sink(paste0("doKNN_", tick, response, "_", metric, ".txt"))
        warningOut <- file(paste0("Warnings/doKNN_", tick, response, "_", metric, "_warnings.txt"), open="wt")
        sink(warningOut, type = "message")
        pdf(paste0('KNNplots_',tick, response, '_', metric, '.pdf'))
        
        ## Flow and plots inspired by and modified from http://blog.yhat.com/posts/comparing-random-forests-in-python-and-r.html
        ## Setup data
        cols <- colnames(train.df)
        cols <- cols[!cols %in% "date"]
        
        ## Create Random Forest Seeds
        # Seeding and timeslice methodology inspired by https://rpubs.com/crossxwill/time-series-cv
        set.seed(123)
        seeds <- vector(mode = "list", length = 79) #Length based on number of resamples + 1 for final model iteration
        for(i in 1:78) seeds[[i]] <- sample.int(1000, 72) #sample.int second argument value based on expand.grid length
        seeds[[79]] <- sample.int(1000, 1)
        
        ## Setup training parameters
        ts.control <- trainControl(method="timeslice", initialWindow = 98, horizon = 7, fixedWindow = FALSE, allowParallel = TRUE, seeds = seeds, search = "grid") #70 hour initial cv training, 35 hour cv testing
        tuneGridKNN <- expand.grid(k=c(1:72))
        #metric <- "Rsquared"
        
        ## Perform training
        start <- Sys.time() #Start timer
        knn <- train(formula, data = train.df[,cols], method = "knn", metric = metric, trControl = ts.control, tuneGrid = tuneGridKNN, importance=TRUE)#, preProcess = "scale")
        print(Sys.time() - start)
        #tuneGridRF <- expand.grid(.mtry=22)
        #rf <- train(high ~ ., data = train.df[,cols], method = "rf", metric = metric, trControl = ts.control, tuneGrid = tuneGridRF, importance=TRUE) #AAPL best mtry = 22
        cat("\nKNN Output\n")
        print(knn)
        print(plot(knn))
        
        ## Evaluate metrics
        r2.train <- rSquared(train.df[,response], train.df[,response] - predict(knn, train.df[,cols]))
        r2.pred <- rSquared(pred.df[,response], pred.df[,response] - predict(knn, pred.df[,cols]))
        mse.train <- mean((train.df[,response] - predict(knn, train.df[,cols]))^2)
        mse.pred <- mean((pred.df[,response] - predict(knn, pred.df[,cols]))^2)
        rmse.train <- sqrt(mse.train)
        rmse.pred <- sqrt(mse.pred)
        mae.train <- mean(abs(train.df[,response] - predict(knn, train.df[,cols])))
        mae.pred <- mean(abs(pred.df[,response] - predict(knn, pred.df[,cols])))
        mape.train <- MAPE(train.df[,response], predict(knn, train.df[,cols]))
        mape.pred <- MAPE(pred.df[,response], predict(knn, pred.df[,cols]))
        
        ## Plot Rsquared Evaluation
        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=train.df[,response], pred=predict(knn, train.df[,cols])))
        print(p + geom_point() +
                  geom_abline(color="red") +
                  ggtitle(paste(tick, response, "KNN Regression: Training r^2 =", r2.train)))
        
        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=pred.df[,response], pred=predict(knn, pred.df[,cols])))
        print(p + geom_point() +
                  geom_abline(color="red") +
                  ggtitle(paste(tick, response, "KNN Regression: Prediction r^2 =", r2.pred)))
        
        if(xform == "diff"){
            ## Plot trained and predicted performance
            plot(as.numeric(c(cumsum(c(orig.train.df[,orig.response][1], train.df[,response])), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response])))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", main = paste0(tick, " KNN Performance (", response, "): Training + Prediction"))
            axis(1, at=1:(sum(length(train.df[,response]), length(pred.df[,response]))), labels=FALSE)
            text(1:(sum(length(train.df[,response]), length(pred.df[,response]))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
            lines(c(cumsum(c(orig.train.df[,orig.response][1], predict(knn, train.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], predict(knn, pred.df[,cols])))), type = "l", lty = 2, lwd = 2, col = "red")
            abline(v = length(train.df[,response])+1, lty = 2, col = "blue")

            ## Plot just predicted performance
            plot(as.numeric(cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response]))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", ylim = c(min(c(cumsum(c(orig.pred.df[,orig.response][1], predict(knn, pred.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response])))), max(c(cumsum(c(orig.pred.df[,orig.response][1], predict(knn, pred.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response]))))), main = paste0(tick, " KNN Performance (", response, "): Prediction"))
            axis(1, at=1:(length(pred.df[,response])), labels=FALSE)
            text(1:(length(pred.df[,response])), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
            lines(cumsum(c(orig.pred.df[,orig.response][1], predict(knn, pred.df[,cols]))), type = "l", lty = 2, lwd = 2, col = "red")
        }
        #else{
        ## Plot trained and predicted performance
        plot(as.numeric(c(train.df[,response], pred.df[,response])), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", main = paste0(tick, " KNN Performance (", response, "): Training + Prediction"))
        axis(1, at=1:(sum(length(train.df[,response]), length(pred.df[,response]))), labels=FALSE)
        text(1:(sum(length(train.df[,response]), length(pred.df[,response]))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
        lines(c(predict(knn, train.df[,cols]),predict(knn, pred.df[,cols])), type = "l", lty = 2, lwd = 2, col = "red")
        abline(v = length(train.df[,response])+1, lty = 2, col = "blue")
        
        ## Plot just predicted performance
        plot(as.numeric(pred.df[,response]), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", ylim = c(min(c(predict(knn, pred.df[,cols]), pred.df[,response])), max(c(predict(knn, pred.df[,cols]), pred.df[,response]))), main = paste0(tick, " KNN Performance (", response, "): Prediction"))
        axis(1, at=1:(length(pred.df[,response])), labels=FALSE)
        text(1:(length(pred.df[,response])), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
        lines(predict(knn, pred.df[,cols]), type = "l", lty = 2, lwd = 2, col = "red")
        #}
        
        ## Get feature importance
        feat.imp <- varImp(knn)
        plot(feat.imp, main = paste(tick, response, "KNN Feature Importance"))
        
        on.exit(dev.off())
        on.exit(sink(type="message"), add = TRUE)
        on.exit(close(warningOut), add = TRUE)
        on.exit(sink(), add = TRUE)
    }, error = function(e){
        on.exit(dev.off())
        on.exit(sink(type="message"), add = TRUE)
        on.exit(close(warningOut), add = TRUE)
        on.exit(sink(), add = TRUE)
        print(paste(tick, response, "RF failed"))
    })
    
    
    return(list(knn, list(r2.train, r2.pred), list(mse.train, mse.pred), list(rmse.train, rmse.pred), list(mae.train, mae.pred), list(mape.train, mape.pred), feat.imp))
}

startOverall <- Sys.time() #Start Overall timer

## Predict 'close.diff'
setup_close.diff()

AAPL.closediff.knn.Rsquared <- doKNN(AAPL.train, AAPL.pred, "AAPL", "close.diff", close.diff ~ ., "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.knn.Rsquared <- doKNN(XOM.train, XOM.pred, "XOM", "close.diff", close.diff ~ ., "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "close")
AAPL.closediff.knn.RMSE <- doKNN(AAPL.train, AAPL.pred, "AAPL", "close.diff", close.diff ~ ., "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.knn.RMSE <- doKNN(XOM.train, XOM.pred, "XOM", "close.diff", close.diff ~ ., "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "close")

#Debug only
# train.df <- AAPL.train
# pred.df <- AAPL.pred
# tick <- "AAPL"
# response = "close.diff"
# orig.response = "close"
# formula = close.diff ~ .
# metric = "Rsquared"
# xform = "diff"
# orig.train.df <- AAPL.train.adder
# orig.pred.df <- AAPL.pred.adder


## Predict 'return.percent'
setup_return.percent()

AAPL.returnpercent.knn.Rsquared <- doKNN(AAPL.train, AAPL.pred, "AAPL", "return.percent", return.percent ~ ., "Rsquared")
XOM.returnpercent.knn.Rsquared <- doKNN(XOM.train, XOM.pred, "XOM", "return.percent", return.percent ~ ., "Rsquared")
AAPL.returnpercent.knn.RMSE <- doKNN(AAPL.train, AAPL.pred, "AAPL", "return.percent", return.percent ~ ., "RMSE")
XOM.returnpercent.knn.RMSE <- doKNN(XOM.train, XOM.pred, "XOM", "return.percent", return.percent ~ ., "RMSE")

## Predict 'volatility.diff'
setup_volatility.diff()

AAPL.volatilitydiff.knn.Rsquared <- doKNN(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", volatility.diff ~ ., "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.knn.Rsquared <- doKNN(XOM.train, XOM.pred, "XOM", "volatility.diff", volatility.diff ~ ., "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "volatility")
AAPL.volatilitydiff.knn.RMSE <- doKNN(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", volatility.diff ~ ., "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.knn.RMSE <- doKNN(XOM.train, XOM.pred, "XOM", "volatility.diff", volatility.diff ~ ., "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "volatility")

print(Sys.time() - startOverall)

## Save models and metrics to external RDS
for(m in ls(pattern = '\\.knn\\.')){
    saveRDS(get(m), paste0('Data/ModelRDS/', m, '.RDS'))
}


####################################################
###### Generate GAM Spline Regression Model ########
####################################################
doGAM <- function(train.df, pred.df, tick, response, formula, metric, xform = "", orig.train.df = train.df, orig.pred.df = pred.df, orig.response = response){
    tryCatch({
        ## Write outputs to external files for later review
        par(mfrow=c(1,1))
        sink(paste0("doGAM_", tick, response, "_", metric, ".txt"))
        warningOut <- file(paste0("Warnings/doGAM_", tick, response, "_", metric, "_warnings.txt"), open="wt")
        sink(warningOut, type = "message")
        pdf(paste0('GAMplots_',tick, response, '_', metric, '.pdf'))
        
        ## Flow and plots inspired by and modified from http://blog.yhat.com/posts/comparing-random-forests-in-python-and-r.html
        ## Setup data
        cols <- colnames(train.df)
        cols <- cols[!cols %in% "date"]
        
        ## Create Random Forest Seeds
        # Seeding and timeslice methodology inspired by https://rpubs.com/crossxwill/time-series-cv
        set.seed(123)
        seeds <- vector(mode = "list", length = 79) #Length based on number of resamples + 1 for final model iteration
        for(i in 1:78) seeds[[i]] <- sample.int(1000, 72) #sample.int second argument value based on expand.grid length
        seeds[[79]] <- sample.int(1000, 1)
        
        ## Setup training parameters
        ts.control <- trainControl(method="timeslice", initialWindow = 98, horizon = 7, fixedWindow = FALSE, allowParallel = TRUE, seeds = seeds, search = "grid") #70 hour initial cv training, 35 hour cv testing
        tuneGridGAM <- expand.grid(df=c(1:72))
        #metric <- "Rsquared"
        
        ## Perform training
        start <- Sys.time() #Start timer
        gam <- train(formula, data = train.df[,cols], method = "gamSpline", metric = metric, trControl = ts.control, tuneGrid = tuneGridGAM, importance=TRUE)#, preProcess = "scale")
        print(Sys.time() - start)
        
        cat("\ngamSpline Output\n")
        print(gam)
        print(plot(gam))
        
        ## Evaluate metrics
        r2.train <- rSquared(train.df[,response], train.df[,response] - predict(gam, train.df[,cols]))
        r2.pred <- rSquared(pred.df[,response], pred.df[,response] - predict(gam, pred.df[,cols]))
        mse.train <- mean((train.df[,response] - predict(gam, train.df[,cols]))^2)
        mse.pred <- mean((pred.df[,response] - predict(gam, pred.df[,cols]))^2)
        rmse.train <- sqrt(mse.train)
        rmse.pred <- sqrt(mse.pred)
        mae.train <- mean(abs(train.df[,response] - predict(gam, train.df[,cols])))
        mae.pred <- mean(abs(pred.df[,response] - predict(gam, pred.df[,cols])))
        mape.train <- MAPE(train.df[,response], predict(gam, train.df[,cols]))
        mape.pred <- MAPE(pred.df[,response], predict(gam, pred.df[,cols]))
        
        ## Plot Rsquared Evaluation
        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=train.df[,response], pred=predict(gam, train.df[,cols])))
        print(p + geom_point() +
                  geom_abline(color="red") +
                  ggtitle(paste(tick, response, "gamSpline Regression: Training r^2 =", r2.train)))
        
        p <- ggplot(aes(x=actual, y=pred),
                    data=data.frame(actual=pred.df[,response], pred=predict(gam, pred.df[,cols])))
        print(p + geom_point() +
                  geom_abline(color="red") +
                  ggtitle(paste(tick, response, "gamSpline Regression: Prediction r^2 =", r2.pred)))
        
        if(xform == "diff"){
            ## Plot trained and predicted performance
            plot(as.numeric(c(cumsum(c(orig.train.df[,orig.response][1], train.df[,response])), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response])))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", main = paste0(tick, " gamSpline Performance (", response, "): Training + Prediction"))
            axis(1, at=1:(sum(length(train.df[,response]), length(pred.df[,response]))), labels=FALSE)
            text(1:(sum(length(train.df[,response]), length(pred.df[,response]))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
            lines(c(cumsum(c(orig.train.df[,orig.response][1], predict(gam, train.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], predict(gam, pred.df[,cols])))), type = "l", lty = 2, lwd = 2, col = "red")
            abline(v = length(train.df[,response])+1, lty = 2, col = "blue")
            
            ## Plot just predicted performance
            plot(as.numeric(cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response]))), type = "l", lty = 1, xlab = "Date & Hour", ylab = paste0("Cumulative Sum of ", orig.response, "[1] and DIff = ", orig.response), xaxt = "n", ylim = c(min(c(cumsum(c(orig.pred.df[,orig.response][1], predict(gam, pred.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response])))), max(c(cumsum(c(orig.pred.df[,orig.response][1], predict(gam, pred.df[,cols]))), cumsum(c(orig.pred.df[,orig.response][1], pred.df[,response]))))), main = paste0(tick, " gamSpline Performance (", response, "): Prediction"))
            axis(1, at=1:(length(pred.df[,response])), labels=FALSE)
            text(1:(length(pred.df[,response])), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
            lines(cumsum(c(orig.pred.df[,orig.response][1], predict(gam, pred.df[,cols]))), type = "l", lty = 2, lwd = 2, col = "red")
        }
        #else{
        ## Plot trained and predicted performance
        plot(as.numeric(c(train.df[,response], pred.df[,response])), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", main = paste0(tick, " gamSpline Performance (", response, "): Training + Prediction"))
        axis(1, at=1:(sum(length(train.df[,response]), length(pred.df[,response]))), labels=FALSE)
        text(1:(sum(length(train.df[,response]), length(pred.df[,response]))), par("usr")[3] - 0.2, labels = c(paste(train.df$date, train.df$hour), paste(pred.df$date, pred.df$hour)), srt = 90, pos = 2, xpd = TRUE, cex = 0.5, offset = -0.1)
        lines(c(predict(gam, train.df[,cols]),predict(gam, pred.df[,cols])), type = "l", lty = 2, lwd = 2, col = "red")
        abline(v = length(train.df[,response])+1, lty = 2, col = "blue")
        
        ## Plot just predicted performance
        plot(as.numeric(pred.df[,response]), type = "l", lty = 1, xlab = "Date & Hour", ylab = response, xaxt = "n", ylim = c(min(c(predict(gam, pred.df[,cols]), pred.df[,response])), max(c(predict(gam, pred.df[,cols]), pred.df[,response]))), main = paste0(tick, " gamSpline Performance (", response, "): Prediction"))
        axis(1, at=1:(length(pred.df[,response])), labels=FALSE)
        text(1:(length(pred.df[,response])), par("usr")[3] - 0.2, labels = paste(pred.df$date, pred.df$hour), srt = 90, pos = 2, xpd = TRUE, cex = 0.6, offset = -0.1)
        lines(predict(gam, pred.df[,cols]), type = "l", lty = 2, lwd = 2, col = "red")
        #}
        
        tryCatch({
            ## Get feature importance
            feat.imp <- varImp(gam)
            plot(feat.imp, main = paste(tick, response, "gamSpline Feature Importance"))
            
            on.exit(dev.off())
            on.exit(sink(type="message"), add = TRUE)
            on.exit(close(warningOut), add = TRUE)
            on.exit(sink(), add = TRUE)
            return(list(gam, list(r2.train, r2.pred), list(mse.train, mse.pred), list(rmse.train, rmse.pred), list(mae.train, mae.pred), list(mape.train, mape.pred), feat.imp))
        }, error = function(e){
            on.exit(dev.off())
            on.exit(sink(type="message"), add = TRUE)
            on.exit(close(warningOut), add = TRUE)
            on.exit(sink(), add = TRUE)
            return(list(gam, list(r2.train, r2.pred), list(mse.train, mse.pred), list(rmse.train, rmse.pred), list(mae.train, mae.pred), list(mape.train, mape.pred), NA))
        })
    }, error = function(e){
        on.exit(dev.off())
        on.exit(sink(type="message"), add = TRUE)
        on.exit(close(warningOut), add = TRUE)
        on.exit(sink(), add = TRUE)
        print(paste(tick, response, "gamSpline failed"))
    })
}

startOverall <- Sys.time() #Start Overall timer

## Predict 'close.diff'
setup_close.diff()

AAPL.closediff.gam.Rsquared <- doGAM(AAPL.train, AAPL.pred, "AAPL", "close.diff", close.diff ~ ., "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.gam.Rsquared <- doGAM(XOM.train, XOM.pred, "XOM", "close.diff", close.diff ~ ., "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "close")
AAPL.closediff.gam.RMSE <- doGAM(AAPL.train, AAPL.pred, "AAPL", "close.diff", close.diff ~ ., "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "close")
XOM.closediff.gam.RMSE <- doGAM(XOM.train, XOM.pred, "XOM", "close.diff", close.diff ~ ., "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "close")

#Debug only
# train.df <- AAPL.train
# pred.df <- AAPL.pred
# tick <- "AAPL"
# response = "close.diff"
# orig.response = "close"
# formula = close.diff ~ .
# metric = "Rsquared"
# xform = "diff"
# orig.train.df <- AAPL.train.adder
# orig.pred.df <- AAPL.pred.adder

## Predict 'return.percent'
setup_return.percent()

AAPL.returnpercent.gam.Rsquared <- doGAM(AAPL.train, AAPL.pred, "AAPL", "return.percent", return.percent ~ ., "Rsquared")
XOM.returnpercent.gam.Rsquared <- doGAM(XOM.train, XOM.pred, "XOM", "return.percent", return.percent ~ ., "Rsquared")
AAPL.returnpercent.gam.RMSE <- doGAM(AAPL.train, AAPL.pred, "AAPL", "return.percent", return.percent ~ ., "RMSE")
XOM.returnpercent.gam.RMSE <- doGAM(XOM.train, XOM.pred, "XOM", "return.percent", return.percent ~ ., "RMSE")

## Predict 'volatility.diff'
setup_volatility.diff()

AAPL.volatilitydiff.gam.Rsquared <- doGAM(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", volatility.diff ~ ., "Rsquared", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.gam.Rsquared <- doGAM(XOM.train, XOM.pred, "XOM", "volatility.diff", volatility.diff ~ ., "Rsquared", "diff", XOM.train.adder, XOM.pred.adder, "volatility")
AAPL.volatilitydiff.gam.RMSE <- doGAM(AAPL.train, AAPL.pred, "AAPL", "volatility.diff", volatility.diff ~ ., "RMSE", "diff", AAPL.train.adder, AAPL.pred.adder, "volatility")
XOM.volatilitydiff.gam.RMSE <- doGAM(XOM.train, XOM.pred, "XOM", "volatility.diff", volatility.diff ~ ., "RMSE", "diff", XOM.train.adder, XOM.pred.adder, "volatility")

print(Sys.time() - startOverall)

## Save models and metrics to external RDS
for(m in ls(pattern = '\\.gam\\.')){
    saveRDS(get(m), paste0('Data/ModelRDS/', m, '.RDS'))
}

###########################################
######### Compare Sentiment Models ########
###########################################

## Compare models for each ticker graphically
compareMods <- function(pat, response){
    pdf(paste0('Comparison_', pat, '.pdf'))

    ## Get resample results for each model
    modList <- ls(pattern = paste0("^", pat, ".", response), envir=.GlobalEnv)
    if(pat == 'PG') modList <- modList[!grepl('^SPG', modList)]
    comp <- lapply(modList, function(x) get(x)[[1]])
    names(comp) <- modList
    comp <- resamples(comp)

    ## Compare metrics of resample results
    trellis.par.set(caretTheme())
    print(dotplot(comp, metric = "Rsquared", main = paste0("Sentiment Model Review (", pat, " ", response, ") - R^2")))
    print(dotplot(comp, metric = "RMSE", main = paste0("Sentiment Model Review (", pat, " ", response, ") - RMSE")))
    print(dotplot(comp, metric = "MAE", main = paste0("Sentiment Model Review (", pat, " ", response, ") - MAE")))

    ## Compare and rank overall metrics
    modInfo <- data.frame(model.name = character(), ticker = character(), r2.train = numeric(), r2.pred = numeric(), mse.train = numeric(), mse.pred = numeric(), rmse.train = numeric(), rmse.pred = numeric(), mae.train = numeric(), mae.pred = numeric(), mape.train = numeric(), mape.pred = numeric())
    for(m in modList){
        modInfo  <-  rbind(modInfo, data.frame(model.name = m, ticker = strsplit(m, '\\.|d')[[1]][1],
                                                 r2.train = get(m)[[2]][[1]], r2.pred = get(m)[[2]][[2]],
                                                 mse.train = get(m)[[3]][[1]], mse.pred = get(m)[[3]][[2]],
                                                 rmse.train = get(m)[[4]][[1]], rmse.pred = get(m)[[4]][[2]],
                                                 mae.train = get(m)[[5]][[1]], mae.pred = get(m)[[5]][[2]],
                                                 mape.train = get(m)[[6]][[1]], mape.pred = get(m)[[6]][[2]]))
    }

    model.name <- modInfo$model.name
    ticker <- modInfo$ticker
    r2.train <- rank(-modInfo$r2.train, ties.method = "min")
    r2.pred <- NA #NA chosen since prediction rSquared values are wonky
    mse.train <- rank(modInfo$mse.train, ties.method = "min")
    mse.pred <- rank(modInfo$mse.pred, ties.method = "min")
    rmse.train <- rank(modInfo$rmse.train, ties.method = "min")
    rmse.pred <- rank(modInfo$rmse.pred, ties.method = "min")
    mae.train <- rank(modInfo$mae.train, ties.method = "min")
    mae.pred <- rank(modInfo$mae.pred, ties.method = "min")
    mape.train <- rank(modInfo$mape.train, ties.method = "min")
    mape.pred <- rank(modInfo$mape.pred, ties.method = "min")

    ## Make final rankings
    modRanks <- data.frame(model.name, ticker, r2.train, r2.pred, mse.train, mse.pred, rmse.train, rmse.pred, mae.train, mae.pred, mape.train, mape.pred)
    modRanks$rank.overall <- rank(rowSums(modRanks[,3:length(modRanks)], na.rm = TRUE), ties.method = "min")
    modRanks$rank.train <- rank(rowSums(modRanks[,c(3,5,7,9,11)], na.rm = TRUE), ties.method = "min")
    modRanks$rank.pred <- rank(rowSums(modRanks[,c(4,6,8,10,12)], na.rm = TRUE), ties.method = "min")


    ## Write to global variables
    assign(paste0('Comparison.', pat, '.', response), modInfo, envir=.GlobalEnv)
    assign(paste0('Ranks.', pat, '.', response), modRanks, envir=.GlobalEnv)

    on.exit(dev.off())

    return(comp)
}

## Compare models for each ticker
compareMods('AAPL', 'closediff')
compareMods('AAPL', 'returnpercent')
compareMods('AAPL', 'volatilitydiff')
compareMods('XOM', 'closediff')
compareMods('XOM', 'returnpercent')
compareMods('XOM', 'volatilitydiff')

###########################################
######### Output data to Tableau ##########
###########################################

# ## Output data for visualization in Tableau
# allTickDF <- rbind(AAPL[[1]], AAPL[[2]]) #First, output non-diffed price and scores
# allTickDF$highDiff <- c(AAPL[[3]][[ncol(AAPL[[3]])]], AAPL[[4]][[ncol(AAPL[[4]])]])
# allTickDF$ticker <- 'AAPL'
# ticks10 <- c('AMZN', 'BA', 'DWDP', 'JNJ', 'JPM', 'NEE', 'PG', 'SPG', 'VZ', 'XOM')
# for(t in ticks10){
#     temp <- rbind(get(t)[[1]], get(t)[[2]])
#     temp$highDiff <- c(get(t)[[3]][[ncol(get(t)[[3]])]], get(t)[[4]][[ncol(get(t)[[4]])]])
#     temp$ticker <- t
#     allTickDF <- rbind(allTickDF, temp)
# }
# rm(t, temp, ticks10)
# write.csv(allTickDF, 'Data/Tableau/allTickDF.csv', row.names = FALSE)
# 
# ## Output feature importance where available
# featureRank <- data.frame(model = character(), ticker = character(), feature = character(), importance = numeric())
# decTrees <- ls(pattern = '.xgb.|.rf.')
# for (m in decTrees){
#     if(!is.na(get(m)[length(get(m))])){
#         temp <- data.frame(model = m, ticker = strsplit(m, '\\.')[[1]][1],
#                            feature = row.names(get(m)[[length(get(m))]][[1]]),
#                            importance = get(m)[[length(get(m))]][[1]])
#         featureRank <- rbind(featureRank, temp)
#     }
# }
# rm(m, temp, decTrees)
# write.csv(featureRank, 'Data/Tableau/featureRank.csv', row.names = FALSE)
# 
# ## Output coefficients and confidence intervals where available
# coeffConfs <- data.frame(model = character(), ticker = character(), variable = character(),
#                           coeff = numeric(), confInt.Lower = numeric(), confInt.Upper = numeric())
# linMods <- ls(pattern = '.lm.')
# for (m in linMods){
#     if(!is.na(get(m)[length(get(m))])){
#         temp <- data.frame(model = m, ticker = strsplit(m, '\\.')[[1]][1],
#                            variable = names(get(m)[[length(get(m))-1]]),
#                            coeff = get(m)[[length(get(m))-1]],
#                            confInt.Lower = get(m)[[length(get(m))]][,1],
#                            confInt.Upper = get(m)[[length(get(m))]][,2])
#         coeffConfs <- rbind(coeffConfs, temp)
#     }
# }
# rm(m, temp, linMods)
# write.csv(coeffConfs, 'Data/Tableau/coeffConfs.csv', row.names = FALSE)
# 
# ## Output metric and ranking tables
# comps <- ls(pattern = '^Comparison\\.')
# modelMetrics <- get(comps[1])
# for(c in comps[-1]){
#     modelMetrics <- rbind(modelMetrics, get(c))
# }
# rm(comps, c)
# write.csv(modelMetrics, 'Data/Tableau/modelMetrics.csv', row.names = FALSE)
# 
# rankings <- ls(pattern = '^Ranks\\.')
# modelRanks <- get(rankings[1])
# for(r in rankings[-1]){
#     modelRanks <- rbind(modelRanks, get(r))
# }
# rm(rankings, r)
# write.csv(modelRanks, 'Data/Tableau/modelRanks.csv', row.names = FALSE)