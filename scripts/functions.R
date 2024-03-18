# load UE grid
load_UE_grid <- function(){
  f <- readRDS("inputs/UE_grid.rds")
  return(f)
}
#
# 
# Import cities
load_cities <- function(){
  f <- readRDS("inputs/cities_RNSA.rds")
  return(f)
}
#
#
# Set local parameters list
set_parameters <- function(grid_tilt = 0.01,
                           initial_time = "2020-06-01-12",
                           final_time = "2020-07-01-12",
                           output_freq = 60
                           ){
  p <- list(
    grid_tilt,
    initial_time,
    final_time,
    difftime(final_time, initial_time, units = "hours"),
    output_freq
  )
  names(p) <- c(
    "grid_tilt",
    "initial_time",
    "final_time",
    "duration",
    "output_freq"
  )
  return(p)
}
#
#
# Generate CONTROL file for HYSPLIT
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
#
#
# Generate SETUP file for HYSPLIT
# Interesting parameter : tout (for "time_out" ; unit = minutes)
# tout = 15 means I'll get the position of my starting points every 15'
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
#
#
# Break a date into a vector
# Input : "yyyy-mm-dd-hh"
# Output : list("yyyy", "mm", "dd", "hh")
time_to_char <- function(time_vector){
  c(substr(time_vector,1,4),substr(time_vector,6,7),
    substr(time_vector,9,10),substr(time_vector,12,13))}
#
#
# Transform the initial point into a grid. Tilt in degree of arc ?
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
#