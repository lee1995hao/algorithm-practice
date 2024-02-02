
#Given a list of intervals representing the takeoff and landing times of airplanes, please calculate the maximum number of airplanes in the sky at the same time.
airplanes <- matrix(c(1,10,2,3,5,8,4,7),ncol = 2, byrow = T)

solution.countOfAirplanes <- function(airplanes) {
  time <- c(5,6)
  air <- 0
  drop <- 0
  for(i in 1:nrow(airplanes)){
    if(airplanes[i,][1] <= time[2]){air = air+1}
  }
  
  for(i in 1:nrow(airplanes)){
    if(airplanes[i,][2] <= time[2]){drop = drop+1}
  }
  
  return(still = air- drop) 
  
}