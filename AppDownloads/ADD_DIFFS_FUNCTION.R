
addLookupDiffs <- function(df) {

    df$ten_min_diff		= df$ten_min_count - df$prev_ten_min_count
    df$ten_min_ip_diff 		= df$ten_min_ip_count - df$prev_ten_min_ip_count
    df$ten_min_app_diff 	= df$ten_min_app_count - df$prev_ten_min_app_count
    df$ten_min_os_diff 		= df$ten_min_os_count - df$prev_ten_min_os_count
    df$ten_min_device_diff 	= df$ten_min_device_count - df$prev_ten_min_device_count
    df$ten_min_channel_diff 	= df$ten_min_channel_count - df$prev_ten_min_channel_count

    df$hour_diff		= df$hour_count - df$prev_hour_count
    df$hour_ip_diff 		= df$hour_ip_count - df$prev_hour_ip_count
    df$hour_app_diff 		= df$hour_app_count - df$prev_hour_app_count
    df$hour_os_diff 		= df$hour_os_count - df$prev_hour_os_count
    df$hour_device_diff 	= df$hour_device_count - df$prev_hour_device_count
    df$hour_channel_diff 	= df$hour_channel_count - df$prev_hour_channel_count

    df$day_diff			= df$hour_count - df$prev_day_count
    df$day_ip_diff 		= df$hour_ip_count - df$prev_day_ip_count
    df$day_app_diff 		= df$hour_app_count - df$prev_day_app_count
    df$day_os_diff 		= df$hour_os_count - df$prev_day_os_count
    df$day_device_diff 		= df$hour_device_count - df$prev_day_device_count
    df$day_channel_diff 	= df$hour_channel_count - df$prev_day_channel_count

    return(df)
}

