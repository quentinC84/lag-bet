library(targets)

tar_option_set(
  tidy_eval = NULL,
  packages = c("dplyr", "vroom", "data.table"),
  error = NULL,
  garbage_collection = T,
  resources = NULL
)

source("scripts/functions.R", encoding = "utf-8")

# load UE grid or RNSA cities
# note: create a global object called "start_points"
input_data <- load_UE_grid()
# load cities()

# Load parameters
parameters <- set_parameters(
  grid_tilt = 0.01,
  initial_time = "2020-06-01-12",
  final_time = "2020-07-01-12",
  output_freq = 60
)

list(
  tar_target(start_points, input_data),
  tar_target(grid, make_grid(start_points, tilt = parameters$grid_tilt)),
  tar_target(CONTROL.make(run.nb = parameters$initial_time,
                          target.t = time_to_char(parameters$initial_time),
                          backward = F,
                          grid %>% dplyr::select(y, x, z),
                          duration = parameters$duration,
                          dir.data = ifelse(initial_time>="2019-06-13","/mnt/BIGDATA/PUBLIC/gfs0p25/",
                                            "/mnt/BIGDATA/PUBLIC/gdas0p5/"),
                          dir.out=main_dir))
)