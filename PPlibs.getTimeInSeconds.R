rm(list = ls())

PPlibs.getTimeInSeconds <- function(time) {
  minutes <- as.numeric(substring(time,4,5))
  hours <- as.numeric(substring(time,1,2))
  seconds <- as.numeric(substring(time,7,8))
  
  timeInSeconds <- hours*3600 + minutes*60 + seconds
  
  print(timeInSeconds)
}