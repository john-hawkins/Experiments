#
# THESE LOOKUP TABLES ARE CREATED IN THE SCRIPT: 
# GENERATE_LOOKUPS.R 

    minute_lookup	       		<- fread("lookups/minute_lookup.csv")
    minute_lookup$V1       		<- NULL
    prev_minute_lookup			<- copy(minute_lookup)
    names(minute_lookup) 		<- c('min_label', 'min_count')
    names(prev_minute_lookup)  		<- c('prev_min_lab', 'prev_min_count')

    ten_minute_lookup           	<- fread("lookups/ten_minute_lookup.csv")
    ten_minute_lookup$V1        	<- NULL
    prev_ten_minute_lookup      	<- copy(ten_minute_lookup)
    penul_ten_minute_lookup      	<- copy(ten_minute_lookup)
    names(ten_minute_lookup)    	<- c('ten_min_label', 'ten_min_count')
    names(prev_ten_minute_lookup)       <- c('prev_ten_min_lab', 'prev_ten_min_count')
    names(penul_ten_minute_lookup)      <- c('penul_ten_min_lab', 'penul_ten_min_count')

    hour_lookup                         <- fread("lookups/hour_lookup.csv")
    hour_lookup$V1                      <- NULL
    prev_hour_lookup            	<- copy(hour_lookup)
    penul_hour_lookup            	<- copy(hour_lookup)
    prev_day_lookup            		<- copy(hour_lookup)
    penul_day_lookup            	<- copy(hour_lookup)
    names(hour_lookup)          	<- c('hour_label', 'hour_count')
    names(prev_hour_lookup)     	<- c('prev_hour_lab', 'prev_hour_count')
    names(penul_hour_lookup)     	<- c('penul_hour_lab', 'penul_hour_count')
    names(prev_day_lookup)              <- c('prev_day_lab', 'prev_day_count')
    names(penul_day_lookup)              <- c('penul_day_lab', 'penul_day_count')

    prev_day_attr_lookup      		<- fread("lookups/hour_attr_lookup.csv")
    prev_day_attr_lookup$V1             <- NULL
    penul_day_attr_lookup          	<- copy( prev_day_attr_lookup )
    names(prev_day_attr_lookup)         <- c('prev_day_lab', 'prev_day_attr')
    names(penul_day_attr_lookup)        <- c('penul_day_lab', 'penul_day_attr')

    ip_ten_minute_lookup                <- fread("lookups/ip_ten_minute_lookup.csv")
    ip_ten_minute_lookup$V1             <- NULL
    prev_ten_minute_ip_lookup   	<- copy(ip_ten_minute_lookup)
    penul_ten_minute_ip_lookup   	<- copy(ip_ten_minute_lookup)
    names(ip_ten_minute_lookup)         <- c('ten_min_label', 'ip', 'ten_min_ip_count')
    names(prev_ten_minute_ip_lookup) 	<- c('prev_ten_min_lab', 'ip', 'prev_ten_min_ip_count')
    names(penul_ten_minute_ip_lookup) 	<- c('penul_ten_min_lab', 'ip', 'penul_ten_min_ip_count')

    hour_ip_lookup              	<- fread("lookups/hour_ip_lookup.csv")
    hour_ip_lookup$V1           	<- NULL
    prev_hour_ip_lookup                 <- copy(hour_ip_lookup)
    penul_hour_ip_lookup                <- copy(hour_ip_lookup)
    prev_day_ip_lookup          	<- copy(hour_ip_lookup)
    penul_day_ip_lookup          	<- copy(hour_ip_lookup)
    names(hour_ip_lookup)               <- c('hour_label', 'ip', 'hour_ip_count')
    names(prev_hour_ip_lookup)  	<- c('prev_hour_lab', 'ip', 'prev_hour_ip_count')
    names(penul_hour_ip_lookup)  	<- c('penul_hour_lab', 'ip', 'penul_hour_ip_count')
    names(prev_day_ip_lookup)   	<- c('prev_day_lab', 'ip', 'prev_day_ip_count')
    names(penul_day_ip_lookup)   	<- c('penul_day_lab', 'ip', 'penul_day_ip_count')

    prev_day_ip_attr_lookup                <- fread("lookups/hour_ip_attr_lookup.csv")
    prev_day_ip_attr_lookup$V1             <- NULL
    penul_day_ip_attr_lookup               <- copy( prev_day_ip_attr_lookup )
    names(prev_day_ip_attr_lookup)         <- c('prev_day_lab', 'ip', 'prev_day_ip_attr')
    names(penul_day_ip_attr_lookup)        <- c('penul_day_lab', 'ip', 'penul_day_ip_attr')

    app_ten_minute_lookup               <- fread("lookups/app_ten_minute_lookup.csv")
    app_ten_minute_lookup$V1    	<- NULL
    prev_ten_minute_app_lookup  	<- copy(app_ten_minute_lookup)
    penul_ten_minute_app_lookup  	<- copy(app_ten_minute_lookup)
    names(app_ten_minute_lookup)        <- c('ten_min_label', 'app', 'ten_min_app_count')
    names(prev_ten_minute_app_lookup) 	<- c('prev_ten_min_lab', 'app', 'prev_ten_min_app_count')
    names(penul_ten_minute_app_lookup) 	<- c('penul_ten_min_lab', 'app', 'penul_ten_min_app_count')

    hour_app_lookup             	<- fread("lookups/hour_app_lookup.csv")
    hour_app_lookup$V1          	<- NULL
    prev_hour_app_lookup                <- copy(hour_app_lookup)
    penul_hour_app_lookup               <- copy(hour_app_lookup)
    prev_day_app_lookup                 <- copy(hour_app_lookup)
    penul_day_app_lookup                <- copy(hour_app_lookup)
    names(hour_app_lookup)              <- c('hour_label', 'app', 'hour_app_count')
    names(prev_hour_app_lookup)         <- c('prev_hour_lab', 'app', 'prev_hour_app_count')
    names(penul_hour_app_lookup)        <- c('penul_hour_lab', 'app', 'penul_hour_app_count')
    names(prev_day_app_lookup)  	<- c('prev_day_lab', 'app', 'prev_day_app_count')
    names(penul_day_app_lookup)  	<- c('penul_day_lab', 'app', 'penul_day_app_count')

    prev_day_app_attr_lookup                <- fread("lookups/hour_app_attr_lookup.csv")
    prev_day_app_attr_lookup$V1             <- NULL
    penul_day_app_attr_lookup               <- copy( prev_day_app_attr_lookup )
    names(prev_day_app_attr_lookup)         <- c('prev_day_lab', 'app', 'prev_day_app_attr')
    names(penul_day_app_attr_lookup)        <- c('penul_day_lab', 'app', 'penul_day_app_attr')

    os_ten_minute_lookup            	<- fread("lookups/os_ten_minute_lookup.csv")
    os_ten_minute_lookup$V1             <- NULL
    prev_ten_minute_os_lookup   	<- copy(os_ten_minute_lookup)
    penul_ten_minute_os_lookup   	<- copy(os_ten_minute_lookup)
    names(os_ten_minute_lookup)         <- c('ten_min_label', 'os', 'ten_min_os_count')
    names(prev_ten_minute_os_lookup) 	<- c('prev_ten_min_lab', 'os', 'prev_ten_min_os_count')
    names(penul_ten_minute_os_lookup) 	<- c('penul_ten_min_lab', 'os', 'penul_ten_min_os_count')

    hour_os_lookup                      <- fread("lookups/hour_os_lookup.csv")
    hour_os_lookup$V1           	<- NULL
    prev_hour_os_lookup                 <- copy(hour_os_lookup)
    penul_hour_os_lookup                <- copy(hour_os_lookup)
    prev_day_os_lookup          	<- copy(hour_os_lookup)
    penul_day_os_lookup          	<- copy(hour_os_lookup)
    names(hour_os_lookup)               <- c('hour_label', 'os', 'hour_os_count')
    names(prev_hour_os_lookup)  	<- c('prev_hour_lab', 'os', 'prev_hour_os_count')
    names(penul_hour_os_lookup)  	<- c('penul_hour_lab', 'os', 'penul_hour_os_count')
    names(prev_day_os_lookup)   	<- c('prev_day_lab', 'os', 'prev_day_os_count')
    names(penul_day_os_lookup)   	<- c('penul_day_lab', 'os', 'penul_day_os_count')

    prev_day_os_attr_lookup                <- fread("lookups/hour_os_attr_lookup.csv")
    prev_day_os_attr_lookup$V1             <- NULL
    penul_day_os_attr_lookup               <- copy( prev_day_os_attr_lookup )
    names(prev_day_os_attr_lookup)         <- c('prev_day_lab', 'os', 'prev_day_os_attr')
    names(penul_day_os_attr_lookup)        <- c('penul_day_lab', 'os', 'penul_day_os_attr')

    device_ten_minute_lookup     	<- fread("lookups/device_ten_minute_lookup.csv")
    device_ten_minute_lookup$V1         <- NULL
    prev_ten_minute_device_lookup  	<- copy(device_ten_minute_lookup)
    penul_ten_minute_device_lookup  	<- copy(device_ten_minute_lookup)
    names(device_ten_minute_lookup)     <- c('ten_min_label', 'device', 'ten_min_device_count')
    names(prev_ten_minute_device_lookup)<- c('prev_ten_min_lab', 'device', 'prev_ten_min_device_count')
    names(penul_ten_minute_device_lookup)<- c('penul_ten_min_lab', 'device', 'penul_ten_min_device_count')

    hour_device_lookup          	<- fread("lookups/hour_device_lookup.csv")
    hour_device_lookup$V1               <- NULL
    prev_hour_device_lookup     	<- copy(hour_device_lookup)
    penul_hour_device_lookup     	<- copy(hour_device_lookup)
    prev_day_device_lookup              <- copy(hour_device_lookup)
    penul_day_device_lookup              <- copy(hour_device_lookup)
    names(hour_device_lookup)   	<- c('hour_label', 'device', 'hour_device_count')
    names(prev_hour_device_lookup)      <- c('prev_hour_lab', 'device', 'prev_hour_device_count')
    names(penul_hour_device_lookup)      <- c('penul_hour_lab', 'device', 'penul_hour_device_count')
    names(prev_day_device_lookup)       <- c('prev_day_lab', 'device', 'prev_day_device_count')
    names(penul_day_device_lookup)       <- c('penul_day_lab', 'device', 'penul_day_device_count')

    prev_day_device_attr_lookup                <- fread("lookups/hour_device_attr_lookup.csv")
    prev_day_device_attr_lookup$V1             <- NULL
    penul_day_device_attr_lookup               <- copy( prev_day_device_attr_lookup )
    names(prev_day_device_attr_lookup)         <- c('prev_day_lab', 'device', 'prev_day_device_attr')
    names(penul_day_device_attr_lookup)        <- c('penul_day_lab', 'device', 'penul_day_device_attr')

    channel_ten_minute_lookup                   <- fread("lookups/channel_ten_minute_lookup.csv")
    channel_ten_minute_lookup$V1                <- NULL
    prev_ten_minute_channel_lookup              <- copy(channel_ten_minute_lookup)
    penul_ten_minute_channel_lookup     	<- copy(channel_ten_minute_lookup)
    names(channel_ten_minute_lookup)    	<- c('ten_min_label', 'channel', 'ten_min_channel_count')
    names(prev_ten_minute_channel_lookup)       <- c('prev_ten_min_lab', 'channel', 'prev_ten_min_channel_count')
    names(penul_ten_minute_channel_lookup) 	<- c('penul_ten_min_lab', 'channel', 'penul_ten_min_channel_count')

    hour_channel_lookup         	<- fread("lookups/hour_channel_lookup.csv")
    hour_channel_lookup$V1              <- NULL
    prev_hour_channel_lookup    	<- copy(hour_channel_lookup)
    penul_hour_channel_lookup    	<- copy(hour_channel_lookup)
    prev_day_channel_lookup     	<- copy(hour_channel_lookup)
    penul_day_channel_lookup     	<- copy(hour_channel_lookup)
    names(hour_channel_lookup)  	<- c('hour_label', 'channel', 'hour_channel_count')
    names(prev_hour_channel_lookup) 	<- c('prev_hour_lab', 'channel', 'prev_hour_channel_count')
    names(penul_hour_channel_lookup) 	<- c('penul_hour_lab', 'channel', 'penul_hour_channel_count')
    names(prev_day_channel_lookup)      <- c('prev_day_lab', 'channel', 'prev_day_channel_count')
    names(penul_day_channel_lookup)      <- c('penul_day_lab', 'channel', 'penul_day_channel_count')

    prev_day_channel_attr_lookup                <- fread("lookups/hour_channel_attr_lookup.csv")
    prev_day_channel_attr_lookup$V1             <- NULL
    penul_day_channel_attr_lookup               <- copy( prev_day_channel_attr_lookup )
    names(prev_day_channel_attr_lookup)         <- c('prev_day_lab', 'channel', 'prev_day_channel_attr')
    names(penul_day_channel_attr_lookup)        <- c('penul_day_lab', 'channel', 'penul_day_channel_attr')

