library(tidyr)
library(data.table)
library(dplyr)

# DATA WAS SAVED IN PREVIOUS SCRIPT
df <- fread("data/train_processed.csv")

# SPLIT AND SAVE FIRST OTHERWISE MEMORY IS A PROBLEM
# SPLIT INTO TWO DATA SETS
# - ONE FOR THE ENCODING TABLES
# - ONE FOR THE ML MODELS
sampleData <- df[ df$click_time < '2017-11-08 10:00:00',  ]
saveRDS(sampleData, "data/data_for_encoders.rds")

encodeData <- df[ df$click_time >= '2017-11-08 10:00:00',  ]
saveRDS(encodeData, "data/train_data_to_encode.rds")

rm(df)
rm(encodeData)
rm(sampleData)

df           <- readRDS("data/data_for_encoders.rds")

# CAST: IP,APP,OS,CHANNEL as CHAR for encoding
df$ip = as.character(df$ip)
df$app = as.character(df$app)
df$device = as.character(df$device)
df$os = as.character(df$os)
df$channel = as.character(df$channel)

# USE THE LOG-ODDS AND RIDDIT ENCODERS
source('../../Scripts/R_Feature_gen/LogOdds.R')
source('../../Scripts/R_Feature_gen/Freqs.R')
source('../../Scripts/R_Feature_gen/ApplyEncoder.R')

columns <- c('ip', 'os', 'app', 'device', 'channel' )
target <- 'is_attributed'

lot             <- getLogOddsTable(df, columns, target)
saveRDS(lot, "lookups/logodds_tables_part1.rds")

freq            <- getFrequencyEncoder(df, columns)
saveRDS(freq, "lookups/freq_tables_part1.rds")

## NOW ADDITIONAL COLUMNS  (BUT CLEAN UP FIRST)
df$V1 <- NULL
df$device <- NULL
df$click_time <- NULL
df$min_label <- NULL
df$ten_min_label     <- NULL
df$hour_label <- NULL
df$ip_channel = paste(df$ip,df$channel,sep='_')
df$app_channel = paste(df$app,df$channel,sep='_')
df$app_ip = paste(df$app,df$ip,sep='_')
df$click_hour = as.character(df$click_hour)

columns <- c( 'ip_channel', 'app_channel', 'app_ip', 'click_hour')
target <- 'is_attributed'

lot2             <- getLogOddsTable(df, columns, target)
saveRDS(lot2, "lookups/logodds_tables_part2.rds")
 
freq2            <- getFrequencyEncoder(df, columns)
saveRDS(freq2, "lookups/freq_tables_part2.rds")

# ############################################################
# LOADING IF NECESSARY
# lot 		<- readRDS("logodds_table.rds")
# freq 		<- readRDS("freq_table.rds")
# #############################################################

rm(df)

# A SECOND ROUND OF ENCODERS

df           <- readRDS("data/train_data_to_encode.rds")
sampleData <- df[ df$click_time < '2017-11-08 16:00:00',  ]
saveRDS(sampleData, "data/data_for_encoders_round2.rds")
encodeData <- df[ df$click_time >= '2017-11-08 16:00:00',  ]
saveRDS(encodeData, "data/train_data_to_encode_round2.rds")

sampleData$ip_channel = paste( sampleData$ip, sampleData$channel,sep='_')
sampleData$app_channel = paste( sampleData$app, sampleData$channel,sep='_')
sampleData$app_ip = paste( sampleData$app, sampleData$ip,sep='_')

columns <- c('ip', 'os', 'app', 'device', 'channel', 'ip_channel', 'app_channel', 'app_ip', 'click_hour' )
target <- 'is_attributed'
 
lot3             <- getLogOddsTable(sampleData, columns, target)
saveRDS(lot3, "lookups/logodds_tables_part3.rds")

freq3            <- getFrequencyEncoder(sampleData, columns)
saveRDS(freq3, "lookups/freq_tables_part3.rds")




