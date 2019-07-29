#########################################################
############             SETUP            ###############
#########################################################

# Clean the environment, and console
rm(list = ls()) 
cat("\f")

# Setup
#install.packages("plotly")
#install.packages("readxl")
library(readxl)
library(plotly)
setwd("~/LeapFrog")
load("course_list.RData")

### TESTING
# testing.data <- data.frame(matrix(NA,ncol=4,nrow=260))
# names(testing.data) <- c("map","iteration","time","distance")
#for (x in 13:13){
#  for (y in 1:20){
#    print(c("Start",x,y))
ptm <- proc.time()
### TESTING

#########################################################
############      TUNABLE PARAMETERS      ###############
#########################################################

# eil51, ts225, pr1002, gr120, rat195, Bays29, Berlin52, Cho130, KroA100, pcb442, pr76, gr48, pma343
game.map <- 4 # Pick which map to use [1,13]
game.max <- 40 # Total number of games played [1,inf)
game.length <-1000 # Iterations in each round [1,inf)

game.players <- 1 # Number of players (0,1]
game.accuracy <- 0.1 # Starting accuracy of the players (0,1]
game.alpha <- game.length/10 # Player reduction curve [0,game.length]

#########################################################
############        INITIALIZATION        ###############
#########################################################

course.name <- course.list$name[game.map]
course.dist <- course.list$distance[game.map]
dist.matrix <- as.matrix(read_excel("TSP Data.xlsx", sheet = course.name, col_names = FALSE))
node.count <- dim(dist.matrix)[1] # Total nodes on map

stop.crit <- FALSE # Set to true in main body when ready to exit
game.count <- 0 # counts the number of rounds played
game.iter <- 0 # counts the number of iterations in a round
tour <- sample(1:node.count)

#########################################################
############      DISTANCE FUNCTION       ###############
#########################################################

dist = function(tour, dist.matrix) { 
  sum(dist.matrix[embed(c(tour, tour[1]), 2)]) 
}
tour.dist <- dist(tour, dist.matrix)
best.dist <- tour.dist




#########################################################
############      LANDING LOOKUP TABLE    ###############
#########################################################

#lookup.init <- rep(NaN, node.count^3) 
#lookup.table <- array(lookup.init, c(node.count, node.count, node.count))

#for (lookup.i in 1:node.count){
#  for (lookup.j in 1:(node.count-1)){
#    if (lookup.j == lookup.i) next
#    for (lookup.k in (lookup.j+1):node.count){
#      if (lookup.k == lookup.i) next
#      lookup.table[lookup.i,lookup.j,lookup.k] <- (dist.matrix[lookup.i,lookup.j]
#                                                   + dist.matrix[lookup.i,lookup.k]
#                                                   - dist.matrix[lookup.j,lookup.k])
#    }
#  }
#}

#########################################################
############    LAND DISTANCE FUNCTION    ###############
#########################################################

land.dist <- function(vector, scalar){
  temp.place <- rep(0, (node.count-ceiling(p)-1+i)) # Create an empty vector to store the differences
  for (j in 1:(node.count-p-2+i)){
    temp.place[j] <- sum(dist.matrix[jumpers[i],tour[j]], # New arc 2
                         dist.matrix[jumpers[i],tour[j+1]], # New arc 3
                         -dist.matrix[tour[j],tour[j+1]]) # Deleted arc 3
  }
  temp.place[(node.count-p-1+i)] <- sum(dist.matrix[jumpers[i],tour[node.count-1-ceiling(p)+i]], # New arc 2
                                        dist.matrix[jumpers[i],tour[1]], # New arc 3
                                        -dist.matrix[tour[node.count-1-ceiling(p)+i],tour[1]]) # Deleted arc 3
  return(temp.place)
}

#########################################################
############          MAIN BODY           ###############
#########################################################

while (stop.crit == FALSE){
  # Update stats
  game.iter <- game.iter + 1
  p <- ceiling(game.players * (node.count - 3) * exp(-game.alpha*(game.iter-1)/game.length))
  if(p==0){p<-1}
  
  # Jump
  jumpers <- sample(1:node.count, size=ceiling(p)) # Choose the jumpers
  tour <- tour[!(tour %in% jumpers)]
  
  # Land 
  for (i in 1:ceiling(p)){
    land.values <- land.dist(tour, jumpers[i]) # Calculate the benefit from each landing spot via land.dist
    #land.values <- lookup.table[cbind(rep(jumpers[i],length(tour)),t(apply(embed(c(tour, tour[1]), 2),1,sort)))] # Calculate the benefit from each landing spot via lookup table
    if(game.iter==1 & game.count>0){
      place.size <- max((node.count-((node.count-3)*game.players))*game.accuracy,1) # Landing accuracy
    } else {
      place.size <- 1 # Land in the best spot possible
    }
    tour.place.rand <- sample(1:place.size)[1] # Pick the landing accuracy value
    tour.place <- order(land.values, decreasing=FALSE)[tour.place.rand] # Pick the landing spot
    if(tour.place < length(land.values)){ # Place jumper[i] in the chosen landing spot
      tour <- c(tour[1:(tour.place)], jumpers[i], tour[(tour.place+1):(node.count-1-ceiling(p)+i)])  
    }else{
      tour <- c(tour,jumpers[i])  
    }
  }
  
  # Recalculate distance
  tour.dist <- dist(tour, dist.matrix)
  
  if (tour.dist < best.dist){
    best.dist <- tour.dist
    tour.best <- tour
  }
  
  # Check round length and game length
  if (game.iter==game.length){
    game.iter <- 0 # Start a new round
    game.count <- game.count + 1 # Count the number of rounds
  }
  
  if (game.count == game.max){
    stop.crit <- TRUE
  }
}

### TESTING
time <- (proc.time()-ptm)[1]
#data.full[(20*(x-1)+y),c(1:3,5)] <- c(x,y,time,best.dist)
print(c(best.dist,course.dist,round(100*(best.dist-course.dist)/course.dist),(1000*time)))
#print(c("End",x,y,best.dist,course.dist,time))
#  }}
#boxplot(time~map,
#        data=testing.data,
#        main="Runtimes for each map",
#        xlab="Map",
#        ylab="Runtime",
#        col="orange",
#        border="brown"
#)
### TESTING

# print(c("optimality gap", best.dist, course.dist,abs(course.dist-best.dist)/course.dist))
