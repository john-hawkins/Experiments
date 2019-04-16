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

train1          <- readRDS("data/train_set_part1.rds")
train2          <- readRDS("data/train_set_part2.rds")
train3          <- readRDS("data/train_set_part3.rds")
train4          <- readRDS("data/train_set_part4.rds")
train5          <- readRDS("data/train_set_part5.rds")
train6          <- readRDS("data/train_set_part6.rds")

valid		<- readRDS("data/train_set_part11.rds")

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

replaceNAs( train1, features, 0)
replaceNAs( train2, features, 0)
replaceNAs( train3, features, 0)
replaceNAs( train4, features, 0)
replaceNAs( train5, features, 0)
replaceNAs( train6, features, 0)

replaceNAs( valid, features, 0)

pos.train1 <- train1[train1$is_attributed==1,]
pos.train2 <- train2[train2$is_attributed==1,]
pos.train3 <- train3[train3$is_attributed==1,]
pos.train4 <- train4[train4$is_attributed==1,]
pos.train5 <- train5[train5$is_attributed==1,]
pos.train6 <- train6[train6$is_attributed==1,]

neg.train <- train1[train1$is_attributed==0,]
neg.train$randGroup <- sample(10, nrow(neg.train), replace=TRUE)
neg.train <- neg.train[neg.train$randGroup==1,]
new.train <- rbind(neg.train, pos.train1, pos.train2, pos.train3, pos.train4, pos.train5, pos.train6 )

pos.valid 		<- valid[valid$is_attributed==1,]
neg.valid 		<- valid[valid$is_attributed==0,]
neg.valid$randGroup 	<- sample(10, nrow(neg.valid), replace=TRUE)
neg.valid 		<- neg.valid[neg.valid$randGroup==1,]
new.valid 		<- rbind(neg.valid, pos.valid)

# Extract just the required columns
just.train	<- new.train[,c(target, features)]
just.valid	<- new.valid[,c(target, features)]

### NOW ADD THE RATIOS
train.rats	<- addLookupRatios(just.train)
valid.rats	<- addLookupRatios(just.valid)
 
train.diffs     <- addLookupDiffs(train.rats)
valid.diffs      <- addLookupDiffs(valid.rats)

train.diffs$ip_channel <- NULL
train.diffs$app_channel <- NULL
train.diffs$app_ip <- NULL

valid.diffs$ip_channel <- NULL
valid.diffs$app_channel <- NULL
valid.diffs$app_ip <- NULL

### GET THE NEW FEATURE LIST WITH THE RATIOS
new.feats	<- names(train.diffs)[!names(train.diffs) %in% c(target)]

#### CLEAN UP THE MEMORY
rm(just.train)
rm(just.valid)
rm(neg.train)
rm(neg.valid)
rm(pos.train1)
rm(pos.train2)
rm(pos.train3)
rm(pos.train4)
rm(train1)
rm(train2)
rm(train3)
rm(train4)
rm(valid)


# PUT DATA INTO H2O FRAMES
#train.h2o	<- as.h2o(train.diffs)
train.h2o	<- as.h2o(temp.final)
valid.h2o	<- as.h2o(valid.diffs)

# TRAIN THE MODEL 
#mod.h2o_xgb      <- h2o.xgboost(x=new.feats, y=target, train.h2o, 
#			validation_frame=valid.h2o, stopping_metric='AUC', stopping_rounds=5, 
#			learn_rate=0.3, max_depth=6, ntrees=100, min_rows=3)

mod.h2o_xgb      <- h2o.xgboost(x=new.feats, y=target, train.h2o, learn_rate=0.3, max_depth=6, ntrees=70, min_rows=2)

# LOAD THE TEST DATA FOR KAGGLE

testfiles <- 4
breakgroups <- 8

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
        testg 		<- test.diffs[test.diffs$randGroup==g,]
        test.h2o 	<- as.h2o(testg)
        preds   	<- h2o.predict(object=mod.h2o_xgb, newdata=test.h2o)
        pred.vals 	<- as.data.frame(preds$p1)
        pred.vals$click_id <- testg$click_id
        results 	<- rbind(results, pred.vals)
        rm(test.h2o)
    }
    rm(test.diffs)
}

results <- results[c(2,1)]
srtd.results <- results[order(results$click_id),]
names(srtd.results) <- c('click_id','is_attributed')
write.csv(srtd.results,'results/experiment02.csv', row.names=F)



removeField <- c("app_freq3", "device", "hour_device_count", "penul_day_attr", "prev_hour_ratio", "hour_diff", "click_hour_freq2", "hour_count", "prev_day_attr", "day_ratio")


