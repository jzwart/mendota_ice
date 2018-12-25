task_names <- function(ice_file){
  ice_data <- readRDS(ice_file)

  years = ice_data$winter_start # tasks

  # generate list of tasks
  tasks <- data_frame(task_name = as.character(years)) # tasks just the number of years
  return(tasks$task_name)
}

calc_delays <- function(delay, start_frame, end_frame){
  paste(paste(sprintf('-d%s "#', delay), seq(start_frame-1, end_frame-1), sep = '') %>%
          paste('"', sep = ''), collapse = " ")
}

magick_delays <- function(delay, frames){
  paste(paste(sprintf('-delay %s -loop 0 2_plot/tmp/ice_year_%s.png', delay, frames)), collapse = " ")
}

create_gif <- function(gif_file, frame_delay, final_delay, task_names=NULL) {

  frame_delay = as.numeric(frame_delay)
  final_delay = as.numeric(final_delay)

  # run imageMagick convert to build a gif
  if(is.null(task_names)) task_names <- '*'
  n_frames = length(task_names)

  png_files <- paste(sprintf('2_plot/tmp/ice_year_%s.png', task_names), collapse=' ')
  tmp_dir <- './2_plot/magick'
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  normal_delay = magick_delays(frame_delay, task_names[1:(n_frames-1)])
  last_delay = magick_delays(final_delay, task_names[n_frames])

  magick_command <- sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB %s %s %s',
    tmp_dir, normal_delay, last_delay, gif_file)
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', magick_command)
  }
  system(magick_command)


  normal_delay = calc_delays(frame_delay, 1, n_frames-1)
  last_delay = calc_delays(final_delay, n_frames, n_frames)

  # simplify the gif with gifsicle - cuts size by about 2/3
  gifsicle_command <- sprintf('gifsicle -b -O3 %s %s %s --colors 256', gif_file, normal_delay, last_delay)
  system(gifsicle_command)
}


create_short_gif <- function(gif_file, frame_delay, final_delay, task_names=NULL) {

  frame_delay = as.numeric(frame_delay)
  final_delay = as.numeric(final_delay)

  task_names = task_names[1:10]

  # run imageMagick convert to build a gif
  if(is.null(task_names)) task_names <- '*'
  n_frames = length(task_names)

  png_files <- paste(sprintf('2_plot/tmp/ice_year_%s.png', task_names), collapse=' ')
  tmp_dir <- './2_plot/magick'
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  normal_delay = magick_delays(frame_delay, task_names[1:(n_frames-1)])
  last_delay = magick_delays(final_delay, task_names[n_frames])

  magick_command <- sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB %s %s %s',
    tmp_dir, normal_delay, last_delay, gif_file)
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', magick_command)
  }
  system(magick_command)


  normal_delay = calc_delays(frame_delay, 1, n_frames-1)
  last_delay = calc_delays(final_delay, n_frames, n_frames)

  # simplify the gif with gifsicle - cuts size by about 2/3
  gifsicle_command <- sprintf('gifsicle -b -O3 %s %s %s --colors 256', gif_file, normal_delay, last_delay)
  system(gifsicle_command)
}
