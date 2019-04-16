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
train7          <- readRDS("data/train_set_part7.rds")
train8          <- readRDS("data/train_set_part8.rds")
train9          <- readRDS("data/train_set_part9.rds")
train10         <- readRDS("data/train_set_part10.rds")

valid		<- readRDS("data/train_set_part12.rds")

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
replaceNAs( train7, features, 0)
replaceNAs( train8, features, 0)

replaceNAs( valid, features, 0)

pos.train1 <- train1[train1$is_attributed==1,]
pos.train2 <- train2[train2$is_attributed==1,]
pos.train3 <- train3[train3$is_attributed==1,]
pos.train4 <- train4[train4$is_attributed==1,]
pos.train5 <- train5[train5$is_attributed==1,]
pos.train6 <- train6[train6$is_attributed==1,]
pos.train7 <- train7[train7$is_attributed==1,]
pos.train8 <- train8[train8$is_attributed==1,]
neg.train <- train1[train1$is_attributed==0,]
neg.train$randGroup <- sample(5, nrow(neg.train), replace=TRUE)
neg.train <- neg.train[neg.train$randGroup==1,]
new.train <- rbind(neg.train, pos.train1, pos.train2, pos.train3, pos.train4, pos.train5)
#new.train <- rbind(neg.train, pos.train1, pos.train2, pos.train3, pos.train4, pos.train5, pos.train6, pos.train7, pos.train8)

pos.valid <- valid[valid$is_attributed==1,]
neg.valid 		<- valid[valid$is_attributed==0,]
neg.valid$randGroup 	<- sample(20, nrow(neg.valid), replace=TRUE)
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

### GET THE NEW FEATURE LIST WITH THE RATIOS
new.feats	<- names(train.diffs)[!names(train.diffs) %in% c(target)]


## THE COMBO FEATURES NEED TO BE FACTORS
enforceFactors <- function(df, cols) {
   for(col in cols) {
      df[,col] <- as.factor(df[,col])
   }
   df
}

facts   <- c("ip_channel", "app_channel", "app_ip")
train.final <- enforceFactors(train.diffs, facts)
valid.final <- enforceFactors(valid.diffs, facts)


# PUT DATA INTO H2O FRAMES
train.h2o	<- as.h2o(train.final)
valid.h2o	<- as.h2o(valid.final)

# TRAIN THE MODEL
mod.h2o_xgb      <- h2o.xgboost(x=new.feats, y=target, train.h2o, 
			validation_frame=valid.h2o, stopping_metric='AUC', stopping_rounds=5, 
			learn_rate=0.3, max_depth=8, ntrees=100, min_rows=5)

# #############################
# META-PARAMETERS TO TEST
# learn_rate (0.3) 0.2 0.1
# max_depth (6) max_depth=8 , max_depth=10
# ntrees (50) ntrees=80, ntrees=100, ntrees=150
# min_rows (1) min_rows=5, min_rows=10
# col_sample_rate=0.7
# sample_rate=0.7
# ################################################


# SAVE THE MODEL
model_path <- h2o.saveModel(object=mod.h2o_xgb, path=getwd(), force=TRUE)

#
# This model created when initiating from R
# model_path="/Users/john.hawkins/Projects/AppDownload/XGBoost_model_R_1525054351095_23"
#
# THIS MODEL CREATE WHEN STARTING THE H2O BINARY INDEPENDENTLY
# model_path="/Users/john.hawkins/Projects/AppDownload/XGBoost_model_R_1525070687643_1"
# LOAD MODEL
# mod.h2o_xgb     <- h2o.loadModel( model_path )
  
# TEST THE MODEL 
test.h2o 	<- as.h2o(test.diffs)
preds    	<- h2o.predict(object=mod.h2o_xgb, newdata=test.h2o)
pred.vals	<-as.data.frame(preds$p1)
realvals	<- as.numeric(as.character(test[[target]]))

library(mltools)
auc_roc(pred.vals$p1, realvals)

#
# [1] 0.9745368

