compile_ice_data <- function(out_file, in_file){

  ice_raw = read.csv(in_file, stringsAsFactors = F) %>% as_tibble() %>%
    mutate(duration = as.numeric(DAYS),
           year = WINTER,
           ice_on = CLOSED,
           ice_off = OPENED) %>%
    select(year,ice_on,ice_off,duration) %>%
    dplyr::filter(!is.na(duration)) %>%
    mutate(winter_start = lapply(ice_raw$year, function(cur){
      strsplit(cur,'-')[[1]][1]
      }) %>% unlist() %>% as.numeric(),
      ice_on_year = case_when(grepl(paste(c('Dec','Nov','Oct'),collapse = '|'),ice_on) ~ winter_start, # ice on year started in winter_start if in Nov, Dec, Oct. Otherwise it was first on in next year
                              TRUE ~ winter_start + 1),
      ice_off_year = case_when(grepl(paste(c('Jan','Feb','Mar'),collapse = '|'),ice_on) ~ winter_start + 1,
      TRUE ~ winter_start),
      ice_on_date = as.Date(paste(ice_on_year,ice_on),format = '%Y %d-%b'),
      ice_off_date = as.Date(paste(ice_off_year, ice_off), format= '%Y %d-%b')) %>%
    arrange(winter_start)

  saveRDS(ice_raw, file = out_file)
}
