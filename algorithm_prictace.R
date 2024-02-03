
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


###light question 
light_R_P <- matrix(c(-3,2,1,2,3,2), ncol = 2 , byrow = T)
range_l <- NULL
for (i in 1:nrow(light_R_P)) {
  range_l <- rbind(range_l, c(light_R_P[i,1] - light_R_P[i,2],  light_R_P[i,1] + light_R_P[i,2]))
}
max_l <- max(range_l)
min_l <- min(range_l)
range_ll <- c(min_l:max_l)
point_l <- rep(0, length(range_ll))

for(j in 1:nrow(range_l)){
  t_l  <- c(range_l[j,1]:range_l[j,2])
  for (i in 1:length(c(min_l:max_l))) {
    if(range_ll[i] %in% t_l){ point_l[i] <- point_l[i] + 1}
  }
  
}
min(range_ll[which(point_l >= 2)])

