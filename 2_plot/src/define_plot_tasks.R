# function to sprintf a bunch of key-value (string-variableVector) pairs, then
# paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  strs <- mapply(function(string, variables) {
    spargs <- if(string == '') list(variables) else c(list(string), as.list(variables))
    do.call(sprintf, spargs)
  }, string=names(args), variables=args)
  paste(strs, collapse=sep)
}


list_tasks <- function(ice_file) {

  ice_data <- readRDS(ice_file)

  years = ice_data$winter_start # tasks

  # generate list of tasks
  tasks <- data_frame(task_name = as.character(years)) # tasks just the number of years

  year_frame <- scipiper::create_task_step(
    step_name = 'year_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('2_plot/tmp/ice_year_%s.png', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "plot_ice_frame(",
        "out_file = target_name,",
        "ice_file = '1_data/out/compiled_ice.rds',",
        "winter_year = I('%s'))"= cur_task$tn)
      }
    )

  step_list <- list(year_frame)

  frame_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=step_list,
    add_complete=FALSE,
    final_steps='year_frame')

  return(frame_task_plan)
}


create_plot_frame_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile,
    task_plan=task_plan,
    include=remake_file,
    packages=c('scipiper', 'tidyr', 'dplyr', 'ggplot2', 'ggExtra', 'cowplot'),
    sources=c(
      '2_plot/src/plot_ice_frame.R'),
    ind_complete = F)
}
