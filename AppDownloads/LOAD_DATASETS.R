library(tidyr)
library(data.table)
library(dplyr)

source('ADD_RATIOS_FUNCTION.R')
source('ADD_DIFFS_FUNCTION.R')

replaceNAs      <- function(df, cols, rplcmnt) {
    for (j in cols) set(df, which(is.na(df[[j]])), j, rplcmnt)
}

features = c("ip", "app", "device", "os", "channel", "click_hour", "ip_channel", "app_channel", "app_ip",
"ip.LO1", "os.LO1", "app.LO1", "device.LO1", "channel.LO1", "naiveBayes_LO1",
"ip_channel.LO2", "app_channel.LO2", "app_ip.LO2", "click_hour.LO2", "naiveBayes_LO2",
"ip.LO3", "os.LO3", "app.LO3", "device.LO3", "channel.LO3", "ip_channel.LO3", "app_channel.LO3", "app_ip.LO3", "naiveBayes_LO3",
"ip_freq1", "os_freq1", "app_freq1", "device_freq1", "channel_freq1",
"ip_channel_freq2", "app_channel_freq2", "app_ip_freq2", "click_hour_freq2",
"ip_freq3", "os_freq3", "app_freq3", "device_freq3", "channel_freq3", "ip_channel_freq3", "app_channel_freq3", "app_ip_freq3", "naiveBayesFinal",
"min_count", "prev_min_count", "ten_min_count", "prev_ten_min_count", "penul_ten_min_count", "hour_count", "prev_hour_count", "penul_hour_count",
"prev_day_count", "penul_day_count", "prev_day_attr", "penul_day_attr",
"ten_min_ip_count", "prev_ten_min_ip_count", "hour_ip_count", "prev_hour_ip_count", "penul_hour_ip_count", "prev_day_ip_count",
"penul_day_ip_count", "prev_day_ip_attr", "penul_day_ip_attr",
"ten_min_app_count", "prev_ten_min_app_count", "hour_app_count", "prev_hour_app_count", "penul_hour_app_count",
"prev_day_app_count", "penul_day_app_count", "prev_day_app_attr", "penul_day_app_attr",
"ten_min_os_count", "prev_ten_min_os_count",
"hour_os_count", "prev_hour_os_count", "penul_hour_os_count", "prev_day_os_count",
"penul_day_os_count", "prev_day_os_attr", "penul_day_os_attr",
"ten_min_device_count", "prev_ten_min_device_count", "hour_device_count",
"prev_hour_device_count", "penul_hour_device_count",
"prev_day_device_count", "penul_day_device_count",
"prev_day_device_attr", "penul_day_device_attr",
"ten_min_channel_count", "prev_ten_min_channel_count",
"hour_channel_count", "prev_hour_channel_count",
"penul_hour_channel_count", "prev_day_channel_count",
"penul_day_channel_count", "prev_day_channel_attr",
"penul_day_channel_attr"
)

target = "is_attributed"

traindatasets 	<- 9
alldatasets	<- 12
validationset	<- 10

getTrainingData <-function() {
    results     <- data.frame()
    for(f in seq(1,traindatasets)) {
        filename        <- paste("data/train_set_part", f, ".rds", sep='')
        train           <- readRDS(filename)
        replaceNAs( train, features, 0)
        results         <- rbind(results, train)
        rm(train)
    }
    return(results)
}

getBalancedTrainingData	<-function() {
    results	<- data.frame()
    for(f in seq(1,traindatasets)) {
        filename 	<- paste("data/train_set_part", f, ".rds", sep='')
        train        	<- readRDS(filename)
        replaceNAs( train, features, 0)
        pos.train 	<- train[train$is_attributed==1,]
        pos.train$wght  <- 1
        results		<- rbind(results, pos.train)
        neg.train 	<- train[train$is_attributed==0,]
        neg.sample	<- neg.train[seq(1,nrow(pos.train)),]
        neg.sample$wght  <- 100
        results         <- rbind(results, neg.sample)
        rm(train)
	rm(pos.train)
	rm(neg.train)
	rm(neg.sample)
    }
    return(results)
}

getDownsampledTrainingData <-function(multi) {
    results     <- data.frame()
    for(f in seq(1,traindatasets)) {
        filename        <- paste("data/train_set_part", f, ".rds", sep='')
        train           <- readRDS(filename)
        replaceNAs( train, features, 0)
        pos.train       <- train[train$is_attributed==1,]
        pos.train$wght  <- 1
        results         <- rbind(results, pos.train)
        neg.train       <- train[train$is_attributed==0,]
        neg.sample      <- neg.train[seq(1,multi*nrow(pos.train)),]
        weight		<- ( nrow(neg.train) / nrow(pos.train) ) / multi
        neg.sample$wght <- weight
        results         <- rbind(results, neg.sample)
        rm(train)
        rm(pos.train)
        rm(neg.train)
        rm(neg.sample)
    }
    return(results)
}

processTrainingData <-function(df) {
    just	<- df[,c(target, features)]
    rats	<- addLookupRatios(just)
    diffs	<- addLookupDiffs(rats)
    diffs$ip_channel <- NULL
    diffs$app_channel <- NULL
    diffs$app_ip <- NULL
    return(diffs)
}

getValidationData <-function() {
    filename   		<- paste("data/train_set_part", validationset, ".rds", sep='')
    valid       	<- readRDS(filename)
    pos.valid 		<- valid[valid$is_attributed==1,]
    neg.valid           <- valid[valid$is_attributed==0,]
    neg.valid$randGroup <- sample(8, nrow(neg.valid), replace=TRUE)
    neg.valid           <- neg.valid[neg.valid$randGroup==1,]
    new.valid           <- rbind(neg.valid, pos.valid)
    rm(valid)
    rm(pos.valid)
    rm(neg.valid)
    return(new.valid)
}

