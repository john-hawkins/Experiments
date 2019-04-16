library(tidyr)
library(data.table)
library(dplyr)

# USE THE LOG-ODDS AND RIDDIT ENCODERS
source('../../Scripts/R_Feature_gen/LogOdds.R')
source('../../Scripts/R_Feature_gen/Freqs.R')
source('../../Scripts/R_Feature_gen/ApplyEncoder.R')

source('LOAD_LOOKUPS.R')

encodeDataset <- function(df) {

    # CAST: IP,APP,OS,CHANNEL as CHAR for encoding
    df$ip = as.character(df$ip)
    df$app = as.character(df$app)
    df$device = as.character(df$device)
    df$os = as.character(df$os)
    df$channel = as.character(df$channel)
    df$click_hour = as.character(df$click_hour)
    df$ip_channel = paste(df$ip,df$channel,sep='_')
    df$app_channel = paste(df$app,df$channel,sep='_')
    df$app_ip = paste(df$app,df$ip,sep='_')

    columns <- c('ip', 'os', 'app', 'device', 'channel')
    freq          <- readRDS("lookups/freq_tables_part1.rds")
    lot           <- readRDS("lookups/logodds_tables_part1.rds")

    columns2 <- c( 'ip_channel', 'app_channel', 'app_ip', 'click_hour')
    freq2          <- readRDS("lookups/freq_tables_part2.rds")
    lot2           <- readRDS("lookups/logodds_tables_part2.rds")
 
    columns3 <- c('ip', 'os', 'app', 'device', 'channel', 'ip_channel', 'app_channel', 'app_ip' )
    freq3          <- readRDS("lookups/freq_tables_part3.rds")
    lot3           <- readRDS("lookups/logodds_tables_part3.rds")

    stage1       <- applyLogOddsTable(df, columns, lot, 'LO1')
    stage2       <- applyLogOddsTable(stage1, columns2, lot2, 'LO2')
    stage3       <- applyLogOddsTable(stage2, columns3, lot3, 'LO3')
    freak1       <- applyEncoder(stage3, columns, freq, 'freq', suffix='freq1')
    freak2       <- applyEncoder(freak1, columns2, freq2, 'freq', suffix='freq2')
    df        	 <- applyEncoder(freak2, columns3, freq3, 'freq', suffix='freq3')

    df$naiveBayesFinal <-  df$naiveBayes_LO1 + df$naiveBayes_LO2 + df$naiveBayes_LO3
 
    # ###########################################################
    # CREATE A DATESTAMP LABELS FOR THE PREVIOUS DAY, HOUR AND 
    # 10 MINUTE BLOCK FOR LOOKUPS TABLES TO GET PREVIOUS VOLUMES
    # ############################################################
    df$click_dt                 <- as.POSIXct(df$click_time)
    df$click_prev_min           <- df$click_dt - 60
    df$click_prev_ten_min       <- df$click_dt - 600
    df$click_penul_ten_min      <- df$click_dt - 1200
    df$click_prev_60_min        <- df$click_dt - 3600
    df$click_penul_60_min        <- df$click_dt - (3600*2)
    df$click_prev_24_hr         <- df$click_dt - (3600*24)
    df$click_penul_24_hr        <- df$click_dt - (3600*48)

    # NOW FORCE THEM TO BE STRING AND TRUNCATE TO MAKE LABELS FOR LOOKUP TABLES
    df$prev_min_lab      	<- substr(as.character(df$click_prev_min),1,16)
    df$prev_ten_min_lab         <- substr(as.character(df$click_prev_ten_min),1,15)
    df$penul_ten_min_lab        <- substr(as.character(df$click_penul_ten_min),1,15)
    df$prev_hour_lab            <- substr(as.character(df$click_prev_60_min),1,13)
    df$penul_hour_lab            <- substr(as.character(df$click_penul_60_min),1,13)
    df$prev_day_lab             <- substr(as.character(df$click_prev_24_hr),1,13)
    df$penul_day_lab            <- substr(as.character(df$click_penul_24_hr),1,13)

    # SET DT TO NULL TO SAVE SPACE
    df$click_dt                 <- NULL
    df$click_prev_min           <- NULL
    df$click_prev_ten_min       <- NULL
    df$click_penul_ten_min      <- NULL 
    df$click_prev_60_min        <- NULL 
    df$click_penul_60_min       <- NULL 
    df$click_prev_24_hr         <- NULL 
    df$click_penul_24_hr        <- NULL 

    # CAST: IP,APP,OS,CHANNEL as NUMERIC 
    # NEEDED FOR LOOKUP JOINS 
    # ALSO ML MODELS (TOO MANY LEVELS)
    # ##############################################################################3
    df$ip = as.numeric(df$ip)
    df$app = as.numeric(df$app)
    df$device = as.numeric(df$device)
    df$os = as.numeric(df$os)
    df$channel = as.numeric(df$channel)
    df$click_hour = as.numeric(df$click_hour)

    # ###############################################################################
    # JOIN AGAINST THE LOOKUP TABLES USING THE VARIOUS REFERENCE LABELS
    # ADD IN VOLUMES OF CLICKS FOR EACH OF THE TIME PERIODS
    # COMBINED WITH THE 5 CATEGORICAL VARIABLES - IP, OS, APP, DEVICE, CHANNEL
    # #################################################################################
    temp.df <- left_join(df, minute_lookup, by=c('min_label'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_minute_lookup, by=c('prev_min_lab'), copy=TRUE )
    temp.df <- left_join(temp.df, ten_minute_lookup, by=c('ten_min_label'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_ten_minute_lookup, by=c('prev_ten_min_lab'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_ten_minute_lookup, by=c('penul_ten_min_lab'), copy=TRUE )
    temp.df <- left_join(temp.df, hour_lookup, by=c('hour_label'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_hour_lookup, by=c('prev_hour_lab'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_hour_lookup, by=c('penul_hour_lab'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_day_lookup, by=c('prev_day_lab'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_lookup, by=c('penul_day_lab'), copy=TRUE )

    temp.df <- left_join(temp.df, prev_day_attr_lookup, by=c('prev_day_lab'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_attr_lookup, by=c('penul_day_lab'), copy=TRUE )

    temp.df <- left_join(temp.df, ip_ten_minute_lookup, by=c('ten_min_label', 'ip'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_ten_minute_ip_lookup, by=c('prev_ten_min_lab', 'ip'), copy=TRUE )
    temp.df <- left_join(temp.df, hour_ip_lookup, by=c('hour_label', 'ip'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_hour_ip_lookup, by=c('prev_hour_lab', 'ip'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_hour_ip_lookup, by=c('penul_hour_lab', 'ip'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_day_ip_lookup, by=c('prev_day_lab', 'ip'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_ip_lookup, by=c('penul_day_lab', 'ip'), copy=TRUE )

    temp.df <- left_join(temp.df, prev_day_ip_attr_lookup, by=c('prev_day_lab', 'ip'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_ip_attr_lookup, by=c('penul_day_lab', 'ip'), copy=TRUE )

    temp.df <- left_join(temp.df, app_ten_minute_lookup, by=c('ten_min_label', 'app'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_ten_minute_app_lookup, by=c('prev_ten_min_lab', 'app'), copy=TRUE )
    temp.df <- left_join(temp.df, hour_app_lookup, by=c('hour_label', 'app'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_hour_app_lookup, by=c('prev_hour_lab', 'app'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_hour_app_lookup, by=c('penul_hour_lab', 'app'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_day_app_lookup, by=c('prev_day_lab', 'app'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_app_lookup, by=c('penul_day_lab', 'app'), copy=TRUE )

    temp.df <- left_join(temp.df, prev_day_app_attr_lookup, by=c('prev_day_lab', 'app'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_app_attr_lookup, by=c('penul_day_lab', 'app'), copy=TRUE )

    temp.df <- left_join(temp.df, os_ten_minute_lookup, by=c('ten_min_label', 'os'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_ten_minute_os_lookup, by=c('prev_ten_min_lab', 'os'), copy=TRUE )
    temp.df <- left_join(temp.df, hour_os_lookup, by=c('hour_label', 'os'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_hour_os_lookup, by=c('prev_hour_lab', 'os'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_hour_os_lookup, by=c('penul_hour_lab', 'os'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_day_os_lookup, by=c('prev_day_lab', 'os'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_os_lookup, by=c('penul_day_lab', 'os'), copy=TRUE )
    
    temp.df <- left_join(temp.df, prev_day_os_attr_lookup, by=c('prev_day_lab', 'os'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_os_attr_lookup, by=c('penul_day_lab', 'os'), copy=TRUE )

    temp.df <- left_join(temp.df, device_ten_minute_lookup, by=c('ten_min_label', 'device'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_ten_minute_device_lookup, by=c('prev_ten_min_lab', 'device'), copy=TRUE )
    temp.df <- left_join(temp.df, hour_device_lookup, by=c('hour_label', 'device'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_hour_device_lookup, by=c('prev_hour_lab', 'device'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_hour_device_lookup, by=c('penul_hour_lab', 'device'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_day_device_lookup, by=c('prev_day_lab', 'device'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_device_lookup, by=c('penul_day_lab', 'device'), copy=TRUE )
    
    temp.df <- left_join(temp.df, prev_day_device_attr_lookup, by=c('prev_day_lab', 'device'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_device_attr_lookup, by=c('penul_day_lab', 'device'), copy=TRUE )

    temp.df <- left_join(temp.df, channel_ten_minute_lookup, by=c('ten_min_label', 'channel'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_ten_minute_channel_lookup, by=c('prev_ten_min_lab', 'channel'), copy=TRUE )
    temp.df <- left_join(temp.df, hour_channel_lookup, by=c('hour_label', 'channel'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_hour_channel_lookup, by=c('prev_hour_lab', 'channel'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_hour_channel_lookup, by=c('penul_hour_lab', 'channel'), copy=TRUE )
    temp.df <- left_join(temp.df, prev_day_channel_lookup, by=c('prev_day_lab', 'channel'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_channel_lookup, by=c('penul_day_lab', 'channel'), copy=TRUE )
    
    temp.df <- left_join(temp.df, prev_day_channel_attr_lookup, by=c('prev_day_lab', 'channel'), copy=TRUE )
    temp.df <- left_join(temp.df, penul_day_channel_attr_lookup, by=c('penul_day_lab', 'channel'), copy=TRUE )

    df <- temp.df

    # SET LABELS TO NULL TO SAVE SPACE
    df$prev_min_lab             <- NULL 
    df$prev_ten_min_lab         <- NULL 
    df$penul_ten_min_lab        <- NULL 
    df$prev_hour_lab            <- NULL 
    df$penul_hour_lab           <- NULL 
    df$prev_day_lab             <- NULL 
    df$penul_day_lab            <- NULL 

    return(df)
}

