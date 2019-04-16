library(tidyr)
library(data.table)
library(dplyr)

source("ENCODE_DATA_FUNCTION.R")

df <- fread('data/test.csv')
#df <- fread('data/test_sample.csv')

df$click_hour           <- substr(df$click_time,12,13)
df$min_label        	<- substr(df$click_time,1,16)
df$ten_min_label        <- substr(df$click_time,1,15)
df$hour_label           <- substr(df$click_time,1,13)

# CAST: IP,APP,OS,CHANNEL as CHAR for encoding
df$ip = as.character(df$ip) 
df$app = as.character(df$app)
df$device = as.character(df$device)
df$os = as.character(df$os)
df$channel = as.character(df$channel)

groups          <- 4
df$randGroup    <- sample(groups, size = nrow(df), replace = TRUE)

for(g in seq(1,groups) ) {
        dset            <- df[ df$randGroup==g,]
        encData         <- encodeDataset(dset)
        spath           <- paste("data/test_set_part", g, ".rds", sep="")
        saveRDS(encData, spath)
        rm(encData)
        rm(dset)
}

