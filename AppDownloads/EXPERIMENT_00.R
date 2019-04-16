library(tidyr)
library(data.table)
library(dplyr)
library(xgboost)

source('ADD_RATIOS_FUNCTION.R')
source('ADD_DIFFS_FUNCTION.R')

replaceNAs      <- function(df, cols, rplcmnt) {
    for (j in cols) set(df, which(is.na(df[[j]])), j, rplcmnt)
}

train1          <- readRDS("data/train_set_part1.rds")

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
replaceNAs( valid, features, 0)

pos.train 		<- train1[train1$is_attributed==1,]
neg.train               <- train1[train1$is_attributed==0,]
neg.train$randGroup     <- sample(4, nrow(neg.train), replace=TRUE)
neg.train               <- neg.train[neg.train$randGroup==1,]
new.train               <- rbind(neg.train, pos.train)

pos.valid 		<- valid[valid$is_attributed==1,]
neg.valid 		<- valid[valid$is_attributed==0,]
neg.valid$randGroup 	<- sample(4, nrow(neg.valid), replace=TRUE)
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


## THE COMBO FEATURES NEED TO BE FACTORS
enforceFactors <- function(df, cols) {
   for(col in cols) {
      df[,col] <- as.factor(df[,col])
   }
   df
}

facts	<- c("ip_channel", "app_channel", "app_ip")
train.final <- enforceFactors(train.diffs, facts)
valid.final <- enforceFactors(valid.diffs, facts)

train.final$ip_channel <- NULL
train.final$app_channel <- NULL
train.final$app_ip <- NULL

#
# NOW BUILD SOME MODELS
#

### GET THE NEW FEATURE LIST WITH THE RATIOS
new.feats	<- names(train.final)[!names(train.final) %in% c(target)]

source('../../Scripts/R_Feature_gen/ModelPipeline.R')

rez.rf	<- runRF_pipeline_01(train.diffs, valid.diffs, target, new.feats)  

rez.glm  <- runGLM_pipeline_01(train.final, valid.final, target, new.feats) 

train.final$is_attributed <- as.numeric(as.character(train.final$is_attributed))
rez.xgb  <- runXGB_pipeline_01(train.final, valid.final, target, new.feats) 



