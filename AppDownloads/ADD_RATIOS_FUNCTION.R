
addLookupRatios <- function(df) {

    df$min_ratio = df$min_count/(df$prev_min_count+1)
    df$ten_min_ratio = df$ten_min_count/(1+df$prev_ten_min_count)
    df$prev_ten_min_ratio = df$prev_ten_min_count/(1+df$penul_ten_min_count)
    df$hour_ratio = df$hour_count/(df$prev_hour_count+1)
    df$prev_hour_ratio = df$prev_hour_count/(df$penul_hour_count+1)
    df$day_ratio = df$hour_count/(df$prev_day_count+1)
    df$prev_day_ratio = df$prev_day_count/(df$penul_day_count+1)

    df$ten_min_ip_ratio = df$ten_min_ip_count / (1+df$ten_min_count)
    df$ten_min_ip_only_ratio = df$ten_min_ip_count / (1+df$prev_ten_min_ip_count)
    df$prev_ten_min_ip_ratio = df$prev_ten_min_ip_count / (1+df$prev_ten_min_count)
    df$hour_ip_ratio = df$hour_ip_count / (1+df$hour_count)
    df$hour_ip_only_ratio = df$hour_ip_count / (1+df$prev_hour_ip_count)
    df$day_ip_only_ratio = df$hour_ip_count / (1+df$prev_day_ip_count)
    df$prev_hour_ip_ratio = df$prev_hour_ip_count / (1+df$prev_hour_count)
    df$prev_day_ip_ratio = df$prev_day_ip_count / (1+df$prev_day_count)

    df$ten_min_app_ratio = df$ten_min_app_count / (1+df$ten_min_count)
    df$ten_min_app_only_ratio = df$ten_min_app_count / (1+df$prev_ten_min_app_count)
    df$prev_ten_min_app_ratio = df$prev_ten_min_app_count / (1+df$prev_ten_min_count)
    df$hour_app_ratio = df$hour_app_count / (1+df$hour_count)
    df$hour_app_only_ratio = df$hour_app_count / (1+df$prev_hour_app_count)
    df$day_app_only_ratio = df$hour_app_count / (1+df$prev_day_app_count)
    df$prev_hour_app_ratio = df$prev_hour_app_count / (1+df$prev_hour_count)
    df$prev_day_app_ratio = df$prev_day_app_count / (1+df$prev_day_count)

    df$ten_min_os_ratio = df$ten_min_os_count / (1+df$ten_min_count)
    df$ten_min_os_only_ratio = df$ten_min_os_count / (1+df$prev_ten_min_os_count)
    df$prev_ten_min_os_ratio = df$prev_ten_min_os_count / (1+df$prev_ten_min_count)
    df$hour_os_ratio = df$hour_os_count / (1+df$hour_count)
    df$hour_os_only_ratio = df$hour_os_count / (1+df$prev_hour_os_count)
    df$day_os_only_ratio = df$hour_os_count / (1+df$prev_day_os_count)
    df$prev_hour_os_ratio = df$prev_hour_os_count / (1+df$prev_hour_count)
    df$prev_day_os_ratio = df$prev_day_os_count / (1+df$prev_day_count)

    df$ten_min_device_ratio = df$ten_min_device_count / (1+df$ten_min_count)
    df$ten_min_device_only_ratio = df$ten_min_device_count / (1+df$prev_ten_min_device_count)
    df$prev_ten_min_device_ratio = df$prev_ten_min_device_count / (1+df$prev_ten_min_count)
    df$hour_device_ratio = df$hour_device_count / (1+df$hour_count)
    df$hour_device_only_ratio = df$hour_device_count / (1+df$prev_hour_device_count)
    df$day_device_only_ratio = df$hour_device_count / (1+df$prev_day_device_count)
    df$prev_hour_device_ratio = df$prev_hour_device_count / (1+df$prev_hour_count)
    df$prev_day_device_ratio = df$prev_day_device_count / (1+df$prev_day_count)

    df$ten_min_channel_ratio = df$ten_min_channel_count / (1+df$ten_min_count)
    df$ten_min_channel_only_ratio = df$ten_min_channel_count / (1+df$prev_ten_min_channel_count)
    df$prev_ten_min_channel_ratio = df$prev_ten_min_channel_count / (1+df$prev_ten_min_count)
    df$hour_channel_ratio = df$hour_channel_count / (1+df$hour_count)
    df$hour_channel_only_ratio = df$hour_channel_count / (1+df$prev_hour_channel_count)
    df$day_channel_only_ratio = df$hour_channel_count / (1+df$prev_day_channel_count)
    df$prev_hour_channel_ratio = df$prev_hour_channel_count / (1+df$prev_hour_count)
    df$prev_day_channel_ratio = df$prev_day_channel_count / (1+df$prev_day_count)

    return(df)
}

