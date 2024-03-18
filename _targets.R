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
f <- load_UE_grid()
# load cities()

list(
  tar_target(start_points, f)
)