rm(list=ls())
gc()
# LOGIN -------------------------------------------------------------------
#
# To run in the terminal :
# ssh qcordeau@147.100.14.120
# (srun -p pcbiom9x --mem=512000 --ntasks=1 --cpus-per-task=40 --pty bash)
# qlogin
# ml geos gdal proj/7.0.1 hysplit R/4.0.0
# R
#
# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(vroom)
library(data.table)
#
#
# FUNCTIONS ---------------------------------------------------------------
# Generate CONTROL & SETUP files for HYSPLIT
CONTROL.make<-function(run.nb,target.t,target.x,duration,
                       dir.data,backward=FALSE,dir.out="./",
                       vetical.motion.option=0,
                       top.of.model.domain=10000){
  filename="CONTROL"
  write.table(rbind(target.t), 
              file = filename, append = FALSE, quote = FALSE, 
              sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = FALSE)
  write.table(nrow(target.x), 
              file = filename, append = TRUE, quote = FALSE, 
              sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = FALSE)
  write.table(target.x, 
              file = filename, append = TRUE, quote = FALSE, 
              sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = FALSE)
  if (backward){
    N = -floor((as.numeric(target.t[4])-duration)/24)+1
  } else { N = floor((as.numeric(target.t[4])+duration)/24)+1  }
  write.table(c(ifelse(backward,-duration,duration),vetical.motion.option,
                top.of.model.domain,N), 
              file = filename, append = TRUE, quote = FALSE, 
              sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = FALSE)
  for(i in 1:N){
    if (backward){
      d=as.Date(paste(target.t[1],"-",target.t[2],"-",
                      target.t[3],sep=""))-(i-1)
      dchar=as.character(d)
    } else {
      d=as.Date(paste(target.t[1],"-",target.t[2],"-",
                      target.t[3],sep=""))+(i-1)
      dchar=as.character(d)  
    }
    
    if (d>=("2019-06-12-00") & d<=("2019-06-12-23")){
      # there is no file for 2019/06/12, neither in gdas0p5 nor in gfs0p25
      # use the first available in gfs, i.e. 20190613_gfs0p25
      file.data=paste(substr(dchar,1,4),substr(dchar,6,7),
                      as.numeric(substr(dchar,9,10))+1,"_gfs0p25",sep="")
      dir.data = "/mnt/BIGDATA/PUBLIC/gfs0p25/"
    } else if( d>=("2019-06-13-00")) { 
      file.data=paste(substr(dchar,1,4),substr(dchar,6,7),
                      substr(dchar,9,10),"_gfs0p25",sep="")
      dir.data = "/mnt/BIGDATA/PUBLIC/gfs0p25/"
    } else if (d<=("2019-06-11-23")) {
      file.data=paste(substr(dchar,1,4),substr(dchar,6,7),
                      substr(dchar,9,10),"_gdas0p5",sep="")          
      dir.data = "/mnt/BIGDATA/PUBLIC/gdas0p5/"
    }
    
    
    write.table(c(dir.data,file.data),
                file = filename, append = TRUE, quote = FALSE, 
                sep = " ", eol = "\n", na = "NA", dec = ".", 
                row.names = FALSE, col.names = FALSE)
  }
  file.out=paste("tdump",run.nb,sep="_")
  write.table(c(dir.out,file.out),
              file = filename, append = TRUE, quote = FALSE, 
              sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = FALSE)
  return(NULL)
}
SETUP.make<-function(tout=60,pres=0,tpot=0,tamb=0,rain=0,
                     mixd=0,relh=0,sphu=0,mixr=0,
                     dswf=0,terr=0){
  x<-paste(
    "&SETUP",
    "tratio = 0.75,",
    "delt = 0.0,",
    "mgmin = 10,",
    "khmax = 9999,",
    "kmixd = 0,",
    "kmsl = 1,", #0, attenzione: su hysplit online ? == 1. Lo imposto a 1
    "kagl = 1,",
    "k10m = 1,",
    "nstr = 0,",
    "mhrs = 9999,",
    "nver = 0,",
    paste0("tout = ",tout, ","),    # Time-step (minutes)
    paste0("tm_pres = ",pres, ","), # Pressure 
    paste0("tm_tpot = ",tpot, ","), # Potential Temperature (K)
    paste0("tm_tamb = ",tamb, ","), # Ambient Temperature (K)
    paste0("tm_rain = ",rain, ","), # Rainfall (mm per hr)
    paste0("tm_mixd = ",mixd, ","), # Mixed Layer Depth (m)
    paste0("tm_relh = ",relh, ","), # Relative Humidity (%)
    paste0("tm_sphu = ",sphu, ","),
    paste0("tm_mixr = ",mixr, ","), 
    paste0("tm_dswf = ",dswf, ","), # Downward Solar Radiation Flux (W/m**2)
    paste0("tm_terr = ",terr, ","), # terrain height (m)
    "dxf = 1.00,",
    "dyf = 1.00,",
    "dzf = 0.00999,", #non sar? questo a fare la differenza
    "messg = 'MESSAGE',",
    "/echo",sep="\n")
  filename="SETUP.CFG"
  write.table(x, 
              file = filename, append = FALSE, quote = FALSE, 
              sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = FALSE)
}
# Break a date into a vector
time_to_char <- function(time_vector){
  c(substr(time_vector,1,4),substr(time_vector,6,7),
    substr(time_vector,9,10),substr(time_vector,12,13))}
# Transform the initial point into a grid. Tilt in "degrés d'arc"
make_grid <- function(data, tilt=0.01){
  k <- 4
  n <- nrow(data)
  data[rep(seq_len(n), each = k), ] %>%
    dplyr::mutate(pos = rep(c("d","h","g","b"), length.out=k*n) )  %>%
    dplyr::mutate(group = rep(1:(nrow(data)), each = k)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(x = ifelse(row_number() == 1, x + tilt, ifelse(row_number() == 3, x - tilt, x)),
                  y = ifelse(row_number() == 2, y + tilt, ifelse(row_number() == 4, y - tilt, y))) %>% 
    as.data.frame() %>%
    dplyr::select(-group)
}
#
# METADATA ----------------------------------------------------------------
# Create a 'meta_data' data frame to keep track of parameters for each object
meta_data <- c()
#
# PARAMETERS --------------------------------------------------------------
# Set the main directory
main_dir <- '/mnt/biometrie/qcordeau/HYSPLIT/'
setwd(main_dir)
# Generate the SETUP.CFG file
SETUP.make(tout = 15) # time_step `tout` in minutes
# Import starting points
start_points <- readRDS("INPUT/UE_grid.rds")
n <- nrow(start_points)
# Generate the grid
q <- 0.01 # Value of the tilt
grid <- make_grid(start_points, q)
# Saving metadata about the grid
meta_data[[1]] <- c(paste0("q = ", as.character(q)) )
names(meta_data) <- "grid"
# Set initial and final time
initial_time <- "2020-06-01-12"   # initial time t (format : yyyy-mm-dd-hh)
final_time <- "2020-06-10-12"     # final time T (format : yyyy-mm-dd-hh)
# Set time step ?
tau <- difftime(final_time, initial_time, units = "hours") %>%
  as.numeric()
# HYSPLIT -----------------------------------------------------------------
## Forward ----------------------------------------------------------------
# Generate the control file
forw <- T
CONTROL.make(run.nb = initial_time,
             target.t = time_to_char(initial_time),
             backward = !forw,
             grid %>% dplyr::select(y, x, z),
             duration = tau,
             dir.data = ifelse(initial_time>="2019-06-13","/mnt/BIGDATA/PUBLIC/gfs0p25/",
                               "/mnt/BIGDATA/PUBLIC/gdas0p5/"),
             dir.out=main_dir)
system("hyts_std")
### wrangling ---------------------------------------------------------------
# Creation and wrangling of "reshaped_tdump_YYYY_MM_DD_HH"
n_lines <- nrow(grid)+floor(tau/24)+5
system(paste0("tail -n +",
              n_lines," ",
              main_dir,
              "/tdump_",
              initial_time,
              " > ",
              main_dir,
              "/reshaped_tdump_", 
              initial_time)
)
# Remove leading zeros
system(paste0("sed 's/^[ \t]*//' -i ",main_dir,"/reshaped_tdump_", initial_time))
# Replace multiple spaces with one
system(paste0("sed 's/[[:blank:]][[:blank:]]*/ /g' -i ",main_dir,"/reshaped_tdump_", initial_time))
# Append heading (select variables of interests)
system(paste0("sed -i '1 i\ ", "TRAJ_NB ","MET_GRID_NB ","YEAR ","MONTH ",
              "DAY ","HOUR ","MINUTE ","FORECAST_HOUR ",
              "AGE ","LATITUDE ","LONGITUDE ","ALTITUDE ",
              "PRESSURE ", "AIR_TEMPERATURE ", "RAINFALL ", "MIXED_LAYER_DEPTH ",
              "TERRAIN_HEIGTH ", "DOWNWARD_SOLAR_RADIATION_FLUX", "' ", 
              main_dir,"/reshaped_tdump_", initial_time))
# Rename the reshaped file
system(paste0("mv ", main_dir,"/reshaped_tdump_", initial_time,
              " ",
              main_dir,"/reshaped_tdump_", initial_time, "_forward"))
# Delete the initial unwrangled "tdump..." file
system(paste0("rm ",main_dir,"/tdump_", initial_time))
# Importation of "reshaped_tdump..." in R
fl <- vroom("reshaped_tdump_2020-06-01-12_forward")
fl <- vroom(paste0(main_dir,"/reshaped_tdump_", initial_time, "_forward"),delim=' ')
fl[,c('MET_GRID_NB', 'PRESSURE',
      'FORECAST_HOUR',
      "AIR_TEMPERATURE", "RAINFALL", "MIXED_LAYER_DEPTH", "TERRAIN_HEIGTH", "DOWNWARD_SOLAR_RADIATION_FLUX")]<-NULL
fl <- as.data.frame(lapply(fl, 2, FUN = as.numeric))
# Trajectory 
traj <- fl %>% 
  arrange(TRAJ_NB) %>%
  tibble() %>%
  mutate(LATITUDE = num(LATITUDE, digits=2)) %>%
  mutate(LONGITUDE = num(LONGITUDE, digits=2))
if(forw){
  traj_forward <- traj
} else{
  traj_backward <- traj
}
# Saving metadata about traj_forward
meta_data[[2]] <- c(list(paste0("initial_time = ", as.character(initial_time)),
                       paste0("final_time = ", as.character(final_time)),
                       paste0("tau = ", as.character(tau)),
                       paste0("forward = ", as.character(forw)))
)
names(meta_data) <- c("grid", "traj_forward")
#
## Backward ----------------------------------------------------------------
# Generate the control file
forw <- F
CONTROL.make(run.nb = initial_time,
             target.t = time_to_char(initial_time),
             backward = !forw,
             grid %>% dplyr::select(y, x, z),
             duration = tau,
             dir.data = ifelse(initial_time>="2019-06-13","/mnt/BIGDATA/PUBLIC/gfs0p25/",
                               "/mnt/BIGDATA/PUBLIC/gdas0p5/"),
             dir.out=main_dir)
system("hyts_std")
### wrangling ---------------------------------------------------------------
# Creation and wrangling of "reshaped_tdump_YYYY_MM_DD_HH"
n_lines <- nrow(grid)+floor(tau/24)+5
system(paste0("tail -n +",
              n_lines," ",
              main_dir,
              "/tdump_",
              initial_time,
              " > ",
              main_dir,
              "/reshaped_tdump_", 
              initial_time)
)
# Remove leading zeros
system(paste0("sed 's/^[ \t]*//' -i ",main_dir,"/reshaped_tdump_", initial_time))
# Replace multiple spaces with one
system(paste0("sed 's/[[:blank:]][[:blank:]]*/ /g' -i ",main_dir,"/reshaped_tdump_", initial_time))
# Append heading (select variables of interests)
system(paste0("sed -i '1 i\ ", "TRAJ_NB ","MET_GRID_NB ","YEAR ","MONTH ",
              "DAY ","HOUR ","MINUTE ","FORECAST_HOUR ",
              "AGE ","LATITUDE ","LONGITUDE ","ALTITUDE ",
              "PRESSURE ", "AIR_TEMPERATURE ", "RAINFALL ", "MIXED_LAYER_DEPTH ",
              "TERRAIN_HEIGTH ", "DOWNWARD_SOLAR_RADIATION_FLUX", "' ", 
              main_dir,"/reshaped_tdump_", initial_time))
# Rename the reshaped file
system(paste0("mv ", main_dir,"/reshaped_tdump_", initial_time,
              " ",
              main_dir,"/reshaped_tdump_", initial_time, "_backward"))
# Delete the initial unwrangled "tdump..." file
system(paste0("rm ",main_dir,"/tdump_", initial_time))
# Importation of "reshaped_tdump..." in R
fl2 <- vroom(paste0(main_dir,"/reshaped_tdump_", initial_time, "_backward"),delim=' ')
fl2[,c('MET_GRID_NB', 'PRESSURE',
      'FORECAST_HOUR',
      "AIR_TEMPERATURE", "RAINFALL", "MIXED_LAYER_DEPTH", "TERRAIN_HEIGTH", "DOWNWARD_SOLAR_RADIATION_FLUX")]<-NULL
fl2 <- as.data.frame(lapply(fl2, 2, FUN = as.numeric))
# Trajectory 
traj2 <- fl2 %>% 
  arrange(TRAJ_NB) %>%
  tibble() %>%
  mutate(LATITUDE = num(LATITUDE, digits=2)) %>%
  mutate(LONGITUDE = num(LONGITUDE, digits=2))
if(forw){
  traj_forward <- traj2
} else{
  traj_backward <- traj2
}
# Saving metadata about traj_backward
meta_data[[3]] <- list(paste0("initial_time = ", as.character(initial_time)),
                       paste0("final_time = ", as.character(final_time)),
                       paste0("tau = ", as.character(tau)),
                       paste0("forward = ", as.character(forw)))
names(meta_data) <- c("grid", "traj_forward", "traj_backward")
#
# SAVING TRAJECTORIES & METADATA ------------------------------------------
saveRDS(traj_forward, "traj_forward.rds")
saveRDS(traj_backward, "traj_backward.rds")
saveRDS(meta_data, "meta_data.rds")
#
# COMPUTATION FTLE --------------------------------------------------------
# We try with only one set of trajectories
traj <- traj[1:10,]
traj
# We create the output vector of FTLE
out_FTLE <- data.frame(
  city = NA,
  FTLE = rep( NA, nrow(traj)/10 )
)
# We create the output matrix
out_mat <- matrix(NA, 2, 2)
# We fill it
## out_mat[1,1] -----
a <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="d") %>%      # x_(i+1, j)(t+T)
  pull(LONGITUDE) %>%       #
  as.double()                    
b <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="g") %>%      # x_(i-1, j)(t+T)
  pull(LONGITUDE) %>%       #
  as.double()
c <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="d") %>%      # x_(i+1, j)(t)
  pull(LONGITUDE) %>%       #
  as.double()
d <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="g") %>%      # x_(i-1, j)(t)
  pull(LONGITUDE) %>%       #
  as.double()               
out_mat[1,1] <- (a - b) / (c - d)
## out_mat[1,2] -----
a <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="h") %>%      # x_(i, j+1)(t+T)
  pull(LONGITUDE) %>%       #
  as.double()                    
b <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="b") %>%      # x_(i, j-1)(t+T)
  pull(LONGITUDE) %>%       #
  as.double()
c <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="h") %>%      # y_(i, j+1)(t)
  pull(LATITUDE) %>%        #
  as.double()
d <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="b") %>%      # y_(i, j-1)(t)
  pull(LATITUDE) %>%        #
  as.double()               
out_mat[1,2] <- (a - b) / (c - d)
## out_mat[2,1] -----
a <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="d") %>%      # y_(i+1, j)(t+T)
  pull(LATITUDE) %>%        #
  as.double()                    
b <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="g") %>%      # y_(i-1, j)(t+T)
  pull(LATITUDE) %>%        #
  as.double()
c <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="d") %>%      # x_(i+1, j)(t)
  pull(LONGITUDE) %>%       #
  as.double()
d <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="g") %>%      # x_(i-1, j)(t)
  pull(LONGITUDE) %>%       #
  as.double()               
out_mat[2,1] <- (a - b) / (c - d)
## out_mat[2,2] -----
a <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="h") %>%      # y_(i, j+1)(t+T)
  pull(LATITUDE) %>%        #
  as.double()                    
b <- traj %>%               
  filter(AGE==1) %>%        #
  filter(POS=="b") %>%      # y_(i, j-1)(t+T)
  pull(LATITUDE) %>%        #
  as.double()
c <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="h") %>%      # y_(i, j+1)(t)
  pull(LATITUDE) %>%        #
  as.double()
d <- traj %>%
  filter(AGE==0) %>%        #
  filter(POS=="b") %>%      # y_(i, j-1)(t)
  pull(LATITUDE) %>%        #
  as.double()               
out_mat[2,2] <- (a - b) / (c - d)
## ... ----
M <- t(out_mat)%*%out_mat
lambda_max <- eigen(M)$values %>% max()
out_FTLE$FTLE <- (1/duration) * log( sqrt(lambda_max)  ) 
#
out_FTLE










