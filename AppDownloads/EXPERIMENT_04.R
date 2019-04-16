library(tidyr)
library(data.table)
library(dplyr)
library(h2o)
h2o.init()

source('LOAD_DATASETS.R')

train		<- getBalancedTrainingData() 
valid		<- getValidationData() 

train.diffs	<- processTrainingData(train)
valid.diffs	<- processTrainingData(valid)

train.diffs$wght <- train$wght

### GET THE NEW FEATURE LIST WITH THE RATIOS
new.feats	<- names(train.diffs)[!names(train.diffs) %in% c(target)]

rm(train)
rm(valid)


# PUT DATA INTO H2O FRAMES
train.h2o	<- as.h2o(train.diffs)
valid.h2o	<- as.h2o(valid.diffs)

# TRAIN THE MODEL
mod.h2o_xgb      <- h2o.xgboost(x=new.feats, y=target, train.h2o)
mod.h2o_xgb      <- h2o.xgboost(x=new.feats, y=target, train.h2o, weights_column='wght')

# TRAIN THE MODEL
mod.h2o_xgb      <- h2o.xgboost(x=new.feats, y=target, train.h2o, weights_column='wght',
                        validation_frame=valid.h2o, stopping_metric='AUC', stopping_rounds=5,
                        learn_rate=0.3, max_depth=7, ntrees=100)


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
write.csv(srtd.results,'results/experiment04.csv', row.names=F)


