library(tidyr)
library(data.table)
library(dplyr)

trn.df <- fread('data/train.csv')
tst.df <- fread('data/test_supplement.csv') 

# DROP SOME COLUMNS AND ADD SOME DUMMY COLUMNS TO MATCH AND DISTINGUISH THE DATASETS

trn.df$attributed_time 	<- NULL
tst.df$click_id 	<- NULL
tst.df$is_attributed = NA

# ADD SOME COLUMNS BEFORE JOINING 
trn.df$click_hour           <- substr(trn.df$click_time,12,13)
trn.df$min_label   	    <- substr(trn.df$click_time,1,16)
trn.df$ten_min_label        <- substr(trn.df$click_time,1,15)
trn.df$hour_label           <- substr(trn.df$click_time,1,13)

write.csv(trn.df, file = "data/train_processed.csv")

tst.df$click_hour           <- substr(tst.df$click_time,12,13)
tst.df$min_label            <- substr(tst.df$click_time,1,16)
tst.df$ten_min_label        <- substr(tst.df$click_time,1,15)
tst.df$hour_label           <- substr(tst.df$click_time,1,13)

df <- rbind(trn.df, tst.df)

write.csv(df, file = "data/fullset_processed.csv")
#

df <- fread("data/fullset_processed.csv")

# #################################################################################
# CREATE ALL OF THE  LOOK-UPS TABLES 
# ##############################################################################################
minute_lookup               <- df %>% group_by(min_label) %>% summarize(counts = n())
names(minute_lookup)        <- c('min_label', 'min_count')
write.csv(minute_lookup, file = "lookups/minute_lookup.csv")

ten_minute_lookup 		<- df %>% group_by(ten_min_label) %>% summarize(counts = n())
names(ten_minute_lookup) 	<- c('ten_min_label', 'ten_min_count')
write.csv(ten_minute_lookup, file = "lookups/ten_minute_lookup.csv")

ten_minute_attr_lookup               <- df %>% group_by(ten_min_label) %>% summarize(ratio = mean(is_attributed) )
names(ten_minute_attr_lookup)        <- c('ten_min_label', 'ten_min_attr_ratio')
write.csv(ten_minute_attr_lookup, file = "lookups/ten_minute_attr_lookup.csv")

hour_lookup 			<- df %>% group_by(hour_label) %>% summarize(counts = n())
names(hour_lookup) 		<- c('hour_label', 'hour_count')
write.csv(hour_lookup, file = "lookups/hour_lookup.csv")

hour_attr_lookup               <- df %>% group_by(hour_label) %>% summarize(ratio = mean(is_attributed) )
names(hour_attr_lookup)        <- c('hour_label', 'hour_attr_ratio')
write.csv(hour_attr_lookup, file = "lookups/hour_attr_lookup.csv")

########### TIME AND IP LOOKUPS
#ip_minute_lookup            <- df %>% group_by(min_label, ip) %>% summarize(counts = n())
#names(ip_minute_lookup)     <- c('ten_min_label', 'ip', 'min_ip_count')
#write.csv(ip_minute_lookup, file = "lookups/ip_minute_lookup.csv")
#############

ip_ten_minute_lookup 		<- df %>% group_by(ten_min_label, ip) %>% summarize(counts = n())
names(ip_ten_minute_lookup) 	<- c('ten_min_label', 'ip', 'ten_min_ip_count')
write.csv(ip_ten_minute_lookup, file = "lookups/ip_ten_minute_lookup.csv")

ip_ten_minute_attr_lookup               <- df %>% group_by(ten_min_label, ip) %>% summarize(ratio = mean(is_attributed) )
names(ip_ten_minute_attr_lookup)        <- c('ten_min_label', 'ip', 'ten_min_ip_attr_ratio')
write.csv(ip_ten_minute_attr_lookup, file = "lookups/ip_ten_minute_attr_lookup.csv")

hour_ip_lookup 			<- df %>% group_by(hour_label, ip) %>% summarize(counts = n())
names(hour_ip_lookup) 		<- c('hour_label', 'ip', 'hour_ip_count')
write.csv(hour_ip_lookup, file = "lookups/hour_ip_lookup.csv")

hour_ip_attr_lookup               <- df %>% group_by(hour_label, ip) %>% summarize(ratio = mean(is_attributed) )
names(hour_ip_attr_lookup)        <- c('hour_label', 'ip', 'hour_ip_attr_ratio')
write.csv(hour_ip_attr_lookup, file = "lookups/hour_ip_attr_lookup.csv")


# TIME AND APP LOOKUPS
app_ten_minute_lookup 		<- df %>% group_by(ten_min_label, app) %>% summarize(counts = n())
names(app_ten_minute_lookup) 	<- c('ten_min_label', 'app', 'ten_min_app_count')
write.csv(app_ten_minute_lookup, file = "lookups/app_ten_minute_lookup.csv")

app_ten_minute_attr_lookup               <- df %>% group_by(ten_min_label, app) %>% summarize(ratio = mean(is_attributed) )
names(app_ten_minute_attr_lookup)        <- c('ten_min_label', 'app', 'ten_min_app_attr_ratio')
write.csv(app_ten_minute_attr_lookup, file = "lookups/app_ten_minute_attr_lookup.csv")

hour_app_lookup 		<- df %>% group_by(hour_label, app) %>% summarize(counts = n())
names(hour_app_lookup) 		<- c('hour_label', 'app', 'hour_app_count')
write.csv(hour_app_lookup, file = "lookups/hour_app_lookup.csv")

hour_app_attr_lookup               <- df %>% group_by(hour_label, app) %>% summarize(ratio = mean(is_attributed) )
names(hour_app_attr_lookup)        <- c('hour_label', 'app', 'hour_app_attr_ratio')
write.csv(hour_app_attr_lookup, file = "lookups/hour_app_attr_lookup.csv")



# TIME AND OS LOOKUP
os_ten_minute_lookup 		<- df %>% group_by(ten_min_label, os) %>% summarize(counts = n())
names(os_ten_minute_lookup) 	<- c('ten_min_label', 'os', 'ten_min_os_count')
write.csv(os_ten_minute_lookup, file = "lookups/os_ten_minute_lookup.csv")

os_ten_minute_attr_lookup               <- df %>% group_by(ten_min_label, os) %>% summarize(ratio = mean(is_attributed) )
names(os_ten_minute_attr_lookup)        <- c('ten_min_label', 'os', 'ten_min_os_attr_ratio')
write.csv(os_ten_minute_attr_lookup, file = "lookups/os_ten_minute_attr_lookup.csv")

hour_os_lookup 			<- df %>% group_by(hour_label, os) %>% summarize(counts = n())
names(hour_os_lookup) 		<- c('hour_label', 'os', 'hour_os_count')
write.csv(hour_os_lookup, file = "lookups/hour_os_lookup.csv")

hour_os_attr_lookup               <- df %>% group_by(hour_label, os) %>% summarize(ratio = mean(is_attributed) )
names(hour_os_attr_lookup)        <- c('hour_label', 'os', 'hour_os_attr_ratio')
write.csv(hour_os_attr_lookup, file = "lookups/hour_os_attr_lookup.csv")


# TIME AND DEVICE LOOKUP
device_ten_minute_lookup 		<- df %>% group_by(ten_min_label, device) %>% summarize(counts = n())
names(device_ten_minute_lookup) 	<- c('ten_min_label', 'device', 'ten_min_device_count')
write.csv(device_ten_minute_lookup, file = "lookups/device_ten_minute_lookup.csv")

device_ten_minute_attr_lookup               <- df %>% group_by(ten_min_label, device) %>% summarize(ratio = mean(is_attributed) )
names(device_ten_minute_attr_lookup)        <- c('ten_min_label', 'device', 'ten_min_device_attr_ratio')
write.csv(device_ten_minute_attr_lookup, file = "lookups/device_ten_minute_attr_lookup.csv")

hour_device_lookup 		<- df %>% group_by(hour_label, device) %>% summarize(counts = n())
names(hour_device_lookup) 	<- c('hour_label', 'device', 'hour_device_count')
write.csv(hour_device_lookup, file = "lookups/hour_device_lookup.csv")

hour_device_attr_lookup               <- df %>% group_by(hour_label, device) %>% summarize(ratio = mean(is_attributed) )
names(hour_device_attr_lookup)        <- c('hour_label', 'device', 'hour_device_attr_ratio')
write.csv(hour_device_attr_lookup, file = "lookups/hour_device_attr_lookup.csv")



# TIME AND CHANNEL LOOKUP
channel_ten_minute_lookup 		<- df %>% group_by(ten_min_label, channel) %>% summarize(counts = n())
names(channel_ten_minute_lookup) 	<- c('ten_min_label', 'channel', 'ten_min_channel_count')
write.csv(channel_ten_minute_lookup, file = "lookups/channel_ten_minute_lookup.csv")

channel_ten_minute_attr_lookup               <- df %>% group_by(ten_min_label, channel) %>% summarize(ratio = mean(is_attributed) )
names(channel_ten_minute_attr_lookup)        <- c('ten_min_label', 'channel', 'ten_min_channel_attr_ratio')
write.csv(channel_ten_minute_attr_lookup, file = "lookups/channel_ten_minute_attr_lookup.csv")

hour_channel_lookup 		<- df %>% group_by(hour_label, channel) %>% summarize(counts = n())
names(hour_channel_lookup) 	<- c('hour_label', 'channel', 'hour_channel_count')
write.csv(hour_channel_lookup, file = "lookups/hour_channel_lookup.csv")

hour_channel_attr_lookup               <- df %>% group_by(hour_label, channel) %>% summarize(ratio = mean(is_attributed) )
names(hour_channel_attr_lookup)        <- c('hour_label', 'channel', 'hour_channel_attr_ratio')
write.csv(hour_channel_attr_lookup, file = "lookups/hour_channel_attr_lookup.csv")

