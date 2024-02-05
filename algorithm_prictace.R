
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



#Calculate the number of subarrays with more 1's than 0's.
v <- c(1, 0, 1, 0, 1)
all_combinations <- list()
for (i in 1:length(v)) {
  all_combinations[[i]] <- combn(v, i)
}


result_1 <- 0
for(i in 1:length(all_combinations)){
  for(j in 1:ncol(all_combinations[[i]])){
    count_1 <- sum(which(all_combinations[[i]][,j] == 1))
    count_0 <- sum(which(all_combinations[[i]][,j] == 0))
    if(count_1 - count_0 > 0){result_1 <- result_1 + 1}
  }
  
}



#Dijkstra

minCost <- function(n, roads, discounts) {
  

  graph <- vector("list", n) 
  for (road in roads) {
    u = road[1] + 1
    v = road[2] + 1
    cost = road[3]
    graph[[u]] <- c(graph[[u]], list(c(v, cost))) 
    graph[[v]] <- c(graph[[v]], list(c(u, cost)))
  }
  

  dist <- rep(Inf, n)
  discounts <- matrix(0, n, discounts+1)  
  dist[1] <- 0
  discounts[1,] <- discounts
  
  q <- c(1)
  while(length(q) > 0){
    

    u <- q[which.min(dist[q])]  
    q <- q[q != u]
    
    for (edge in graph[[u]]) {
      v <- edge[[1]]
      length <- edge[[2]]
      

      alt = dist[u] + length
      if (alt < dist[v]) {
        dist[v] <- alt
        q <- c(q, v) 
      }
      

      if (discounts[u,1] > 0 && 
          alt - length/2 < dist[v, discounts[u,1]-1]) {
        
        dist[v, discounts[u,1]-1] <- alt - length/2
        discounts[v, discounts[u,1]-1] <- discounts[u,1] - 1
        q <- c(q, v)
      }
    }
  }
  
  if(is.infinite(dist[n])){
    return(-1)
  } else {
    return(dist[n]) 
  }
}



n <- 4
roads <- matrix(c(0, 1, 100, 
                  0, 2, 300, 
                  1, 3, 600, 
                  2, 3, 200), byrow = TRUE, ncol = 3)
discounts <- 1

result <- minCost(n, roads, discounts)
print(result)


