plot_ice_frame <- function(out_file, ice_file, winter_year){

  ice_all = readRDS(ice_file) %>%
    mutate(ice_on_doy = lubridate::yday(ice_on_date),
           ice_off_doy = lubridate::yday(ice_off_date))

  water_day_lookup = data_frame(water_day = 1:365,
                         date = seq.Date(as.Date('2001-10-01'),as.Date('2002-09-30'), by = 'day'),
                         doy = lubridate::yday(date)) %>%
    select(-date)

  ice_all = left_join(ice_all, water_day_lookup, by = c('ice_on_doy' = 'doy')) %>%
    dplyr::rename(ice_on_water_day = water_day) %>%
    left_join(water_day_lookup, by = c('ice_off_doy' = 'doy')) %>%
    dplyr::rename(ice_off_water_day = water_day) %>%
    gather(ice_event, water_day, contains('water_day'))

  # ice = ice_all %>%
  #   dplyr::filter(winter_start <= winter_year)

  g = ggplot(ice_all, aes(x = water_day, y = duration, color = winter_start, group = winter_start)) +
    geom_line(size = 2, alpha = 0) + # making transparent so the entire color scale is plotted but not data that is not yet
    geom_point(size = 3, alpha = 0) +
    geom_line(data = dplyr::filter(ice_all, winter_start <= winter_year),
              aes(x = water_day, y = duration, color = winter_start), alpha = .7, size = 2) +
    geom_point(data = dplyr::filter(ice_all, winter_start <= winter_year),
               aes(x = water_day, y = duration, color = winter_start), alpha = .7, size = 3) +
    geom_line(data = dplyr::filter(ice_all, winter_start == winter_year),
              aes(x = water_day, y = duration, color = winter_start), alpha = 1, size = 6) +
    geom_point(data = dplyr::filter(ice_all, winter_start == winter_year),
              aes(x = water_day, y = duration, color = winter_start), alpha = 1, size = 8) +
    theme_classic() +
    theme(axis.title = element_text(size =14),
          axis.text = element_text(size = 14),
          axis.title.x = element_blank())+
    ylab('Ice Duration (days)') +
    scale_color_viridis_c(option = 'plasma')

  # ggExtra::ggMarginal(p = g)

  ggsave(filename = out_file, g, height = 8, width = 8, units = 'in')
}
