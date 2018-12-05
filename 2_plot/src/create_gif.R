task_names <- function(ice_file){
  ice_data <- readRDS(ice_file)

  years = ice_data$winter_start # tasks

  # generate list of tasks
  tasks <- data_frame(task_name = as.character(years)) # tasks just the number of years
  return(tasks$task_name)
}

create_gif <- function(gif_file, frame_delay, task_names=NULL) {

  frame_delay = as.numeric(frame_delay)

  # run imageMagick convert to build a gif
  if(is.null(task_names)) task_names <- '*'
  png_files <- paste(sprintf('2_plot/tmp/ice_year_%s.png', task_names), collapse=' ')
  magick_command <- sprintf('magick convert -delay %d -loop 0 %s %s',
                            frame_delay, png_files, gif_file)
  system(magick_command)

  # simplify the gif with gifsicle - cuts size by about 2/3
  gifsicle_command <- sprintf('gifsicle -b -O3 %s --colors 256', gif_file)
  system(gifsicle_command)
}

