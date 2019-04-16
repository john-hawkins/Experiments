
source('LOAD_DATASETS.R')

fulld		<- getTrainingData()        
train           <- getBalancedTrainingData()  
valid           <- getValidationData()  
        
train.diffs     <- processTrainingData(train)
valid.diffs     <- processTrainingData(valid)

target = 'is_attributed'

features = c("ip.LO1", "os.LO1", "app.LO1", "device.LO1", "channel.LO1", "naiveBayes_LO1",
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

new.feats       <- names(train.diffs)[!names(train.diffs) %in% c(target)]

meanvec <- c()

for(col in new.feats) {
    meanvec[[col]] <- mean(train.diffs[,col])
}

stdvec <- c()

for(col in new.feats) {
    stdvec[[col]] <- sd(train.diffs[,col])
}

normalised <- copy(train.diffs)
for(col in new.feats) {
    normalised[[col]] <- ( normalised[[col]] - meanvec[[col]]) / stdvec[[col]]
}

normal.valid <- copy(valid.diffs)
for(col in new.feats) {
    normal.valid[[col]] <- ( normal.valid[[col]] - meanvec[[col]]) / stdvec[[col]]
}

library(glmnet)

x       <- as.matrix(normalised[,new.feats])
y	<- as.factor(normalised[,target])

modcv	<- cv.glmnet(x, y, family="binomial")
preds <- predict(modcv, newx=as.matrix(normal.valid[,new.feats]), type='response')


mod1 <- glmnet(x, y, family="binomial", alpha=0, lambda=1)
preds <- predict(mod1, newx=as.matrix(normal.valid[,new.feats]), type='response')

mod2 <- glmnet(x, y, family="binomial", alpha=0.5, lambda=1)
preds <- predict(mod2, newx=as.matrix(normal.valid[,new.feats]), type='response')

# EVALUATE
library(mltools)

pred.vals       <- as.data.frame(preds)
realvals        <- as.numeric(as.character(valid[[target]]))
auc_roc(pred.vals, realvals)



