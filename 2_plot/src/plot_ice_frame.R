plot_ice_frame <- function(out_file, ice_file, winter_year){

  ice_all = readRDS(ice_file) %>%
    mutate(ice_on_doy = lubridate::yday(ice_on_date),
           ice_off_doy = lubridate::yday(ice_off_date))

  water_day_lookup = data_frame(water_day = 1:365,
                         date = seq.Date(as.Date('2001-10-01'),as.Date('2002-09-30'), by = 'day'),
                         doy = lubridate::yday(date)) #%>%
   # select(-date)

  ice_all = left_join(ice_all, water_day_lookup, by = c('ice_on_doy' = 'doy')) %>%
    dplyr::rename(ice_on_water_day = water_day) %>%
    left_join(water_day_lookup, by = c('ice_off_doy' = 'doy')) %>%
    dplyr::rename(ice_off_water_day = water_day) %>%
    gather(ice_event, water_day, contains('water_day'))

  dist_year = as.numeric(winter_year) - 20 # how many years prior to show trend in distribution

  ice_on_df = ice_all %>%
    dplyr::filter(winter_start > dist_year, winter_start <= winter_year) %>%
    spread(ice_event, water_day) %>%
    select(winter_start, ice_off_water_day, ice_on_water_day)

  water_day_seq = seq(min(ice_on_df$ice_on_water_day), max(ice_on_df$ice_off_water_day))

  ice_on_df = tidyr::crossing(ice_on_df, water_day_seq) %>%
    group_by(winter_start) %>%
    mutate(ice = case_when(water_day_seq >= ice_on_water_day & water_day_seq <= ice_off_water_day ~ 1,
                           TRUE ~ 0)) %>%
    dplyr::filter(ice == 1)


  # ice = ice_all %>%
  #   dplyr::filter(winter_start <= winter_year)
  wd_breaks = c(32, 62, 93, 124, 152, 183, 213)

  main = ggplot(ice_all, aes(x = water_day, y = duration, color = winter_start, group = winter_start)) +
    geom_line(size = 2, alpha = 0) + # making transparent so the entire color scale is plotted but not data that is not yet
    geom_line(data = dplyr::filter(ice_all, winter_start <= winter_year),
              aes(x = water_day, y = duration, color = winter_start), alpha = .5, size = 2) +
    geom_line(data = dplyr::filter(ice_all, winter_start == winter_year),
              aes(x = water_day, y = duration, color = winter_start), alpha = 1, size = 6) +
    theme_classic() +
    theme(axis.title = element_text(size =14),
          axis.text = element_text(size = 14),
          axis.title.x = element_blank(),
          legend.position = c(.1,.15),
          legend.text = element_text(size = 12),
          legend.title = element_blank())+
    ylab('Ice Duration (days)') +
    scale_color_viridis_c(option = 'plasma', direction = -1, breaks = c(1855, 1935, 2015), trans= 'reverse') +
    annotate('text', x = 200, y = 30, label = winter_year, size = 8) +
    scale_x_continuous(name = '', breaks = wd_breaks, labels = strftime(water_day_lookup$date[wd_breaks], format = '%b'))

  main

  example = ggplot(dplyr::filter(ice_all, winter_start == 1950), aes(x = water_day, y = duration, group = winter_start)) +
    geom_line(size = 2, alpha = .5, show.legend = F, color ='darkblue') +
    theme_classic() +
    theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  axis.title = element_blank())+
    xlim(layer_scales(main)$x$range$range) +
    ylim(120,125) +
    ggrepel::geom_text_repel(aes(x = water_day, y = duration, label = c('Ice On','Ice Off')),
                           segment.size  = 1, color = 'grey',
                           size = 5,
                           nudge_y = c(2,2),
                           segment.color = "grey",
                           arrow = arrow(length = unit(.1,'inches'), type = 'open'),
                           direction     = "both")

  example


  out = ggdraw() +
    draw_plot(main, x = 0, y = 0, width = 1, height = .8) +
    draw_plot(example, x = 0, y = .8, width = 1, height = .2)

   out

  ggsave(filename = out_file, out, height = 7, width = 7, units = 'in', dpi = 200)
}
