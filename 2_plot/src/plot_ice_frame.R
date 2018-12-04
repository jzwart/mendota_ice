plot_ice_frame <- function(out_file, ice_file, winter_year){

  ice = readRDS(ice_file) %>%
    dplyr::filter(winter_start <= winter_year) %>%
    mutate(ice_on_doy = lubridate::yday(ice_on_date),
           ice_off_doy = lubridate::yday(ice_off_date))


  water_day_lookup = data_frame(water_day = 1:365,
                         date = seq.Date(as.Date('2001-10-01'),as.Date('2002-09-30'), by = 'day'),
                         doy = lubridate::yday(date)) %>%
    select(-date)

  ice = left_join(ice, water_day_lookup, by = c('ice_on_doy' = 'doy')) %>%
    dplyr::rename(ice_on_water_day = water_day) %>%
    left_join(water_day_lookup, by = c('ice_off_doy' = 'doy')) %>%
    dplyr::rename(ice_off_water_day = water_day) %>%
    gather(ice_event, water_day, contains('water_day'))

  g = ggplot(ice, aes(x = water_day, y = duration, color = winter_start, group = winter_start)) +
    geom_line(size = 2, alpha = .8) +
    geom_point(size = 3) +
    # geom_line(dplyr::filter(ice, winter_start == winter_year),
    #           aes(x = water_day, y = duration)) +
    theme_classic() +
    # scale_color_viridis_c(option = 'plasma')+
    scale_color_gradient(low = 'blue', high = 'yellow')


  g

}
