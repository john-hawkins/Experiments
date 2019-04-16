library(tidyr)
library(data.table)
library(dplyr)
library(h2o)
h2o.init()

source('ADD_RATIOS_FUNCTION.R')
source('ADD_DIFFS_FUNCTION.R')

replaceNAs      <- function(df, cols, rplcmnt) {
    for (j in cols) set(df, which(is.na(df[[j]])), j, rplcmnt)
}

# BUILD PREDICTIVE MODEL
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


dataset		<- 12
testfiles <- 4
breakgroups <- 8

# ITERATE OVER ALL SUBSETS OF DATA
for(d in seq(1,dataset)) {
    filetrain <- paste("data/train_set_part", d, ".rds", sep='')
    train           <- readRDS(filetrain)
    replaceNAs( train, features, 0)
    just.train      <- train[,c( target, features)]
    train.rats      <- addLookupRatios(just.train)
    train.diffs     <- addLookupDiffs(train.rats)
    train.diffs$ip_channel <- NULL
    train.diffs$app_channel <- NULL
    train.diffs$app_ip <- NULL
    rm(train)
    rm(just.train)
    rm(train.rats)
    new.feats       <- names(train.diffs)[!names(train.diffs) %in% c(target)]
    train.h2o       <- as.h2o(train.diffs)
    mod.h2o_xgb      <- h2o.xgboost(x=new.feats, y=target, train.h2o)

    # NOW SCORE THE TEST SET AND SAVE
    results <- data.frame()

for(f in seq(1,testfiles)) {
    filename <- paste("data/test_set_part", f, ".rds", sep='')
    test           <- readRDS(filename)
    replaceNAs( test, features, 0)
    just.test      <- test[,c('click_id', features)]
    test.rats      <- addLookupRatios(just.test)
    test.diffs     <- addLookupDiffs(test.rats)
    test.diffs$ip_channel <- NULL
    test.diffs$app_channel <- NULL
    test.diffs$app_ip <- NULL
    test.diffs$randGroup <- sample(breakgroups, nrow(test.diffs), replace=TRUE)
    rm(test)
    rm(just.test)
    rm(test.rats)
    for(g in seq(1,breakgroups)) {
        testg           <- test.diffs[test.diffs$randGroup==g,]
        test.h2o        <- as.h2o(testg)
        preds           <- h2o.predict(object=mod.h2o_xgb, newdata=test.h2o)
        pred.vals       <- as.data.frame(preds$p1)
        pred.vals$click_id <- testg$click_id
        results         <- rbind(results, pred.vals)
        rm(test.h2o)
    }
    rm(test.diffs)
}

    resultsfile <- paste("results/experiment05_model", d, ".csv", sep='')
    results <- results[c(2,1)]
    srtd.results <- results[order(results$click_id),]
    write.csv(srtd.results, resultsfile, row.names=F)
}


