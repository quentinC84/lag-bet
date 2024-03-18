library(targets)

tar_option_set(
  tidy_eval = NULL,
  packages = c("ssh", "qs", "dplyr", "vroom", "data.table"),
  error = NULL,
  garbage_collection = T,
  resources = NULL
)

source("scripts/functions.R", encoding = "utf-8")

# load UE grid or RNSA cities
input_data <- load_UE_grid()
# load cities()

# Load parameters
parameters <- set_parameters(
  grid_tilt = 0.01,
  initial_time = "2020-06-01-12",
  final_time = "2020-07-01-12",
  output_freq = 60
)
# Available parameters : grid_tilt, initial_time, final_time, duration, output_freq

list(
  #
  tar_target(start_points,
             input_data,
             format = "qs",
             description = "Import of the starting points"),
  #
  tar_target(grid, 
             make_grid(start_points, tilt = parameters$grid_tilt),
             format = "qs",
             description = "Creation of the grid"),
  tar_target(CONTROL,
             CONTROL.make(run.nb = parameters$initial_time,
                          target.t = time_to_char(parameters$initial_time),
                          backward = F,
                          grid %>% dplyr::select(y, x, z),
                          duration = parameters$duration,
                          dir.data = ifelse(initial_time>="2019-06-13","/mnt/BIGDATA/PUBLIC/gfs0p25/",
                                            "/mnt/BIGDATA/PUBLIC/gdas0p5/")),
             format = "qs",
             description = "Create CONTROLE file"
             ),
  tar_target(RUN_FORWARD,
             system("ssh qcordeau@147.100.14.120"),
             format = "qs",
             description = "Run HYSPLIT")
)