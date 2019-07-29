# Clean the environment, and console
  rm(list = ls()) 
  cat("\f")
  # eil51, ts225, pr1002, gr120, rat195, Bays29, Berlin52, Ch130, KroA100, pcb442, pr76, gr48, pma343
  
# Setup
  library(readxl)
  library(plotly)
  setwd("~/Documents/Brandon/AFIT/2019_Q2/OPER_623 Heuristic Search Methods/Project/leapfrog")
  load("course_list.RData")
 

    #########################################################
    ############      TUNABLE PARAMETERS      ###############
    #########################################################

      game.map <- 4 # Pick which map to use [1,13]
      game.players <- 70 # Number of players [1, number of nodes]
      game.accuracy <- 0.2 # Starting accuracy of the players (0,1]
      game.length <- 150 # Iterations in each round
      game.max <- 10 # Total number of games played
      land.high <- 0.01
      land.med <- 0.05
      
# Initialization
      course.name <- course.list$name[game.map]
      course.dist <- course.list$distance[game.map]
      distance.matrix <- as.matrix(read_excel("TSP Data.xlsx", sheet = course.name, col_names = FALSE))
      node.count <- dim(distance.matrix)[1] # Total nodes on map
      best.dist <- 10^10 # Set higher than any random tour would create
      plot.dist <- data.frame(matrix(ncol=3,nrow=1)) # Used to track the tour lengths over time
        names(plot.dist) <- c("iteration", "distance", "optimal")
      stop.crit <- FALSE # Set to true in main body when ready to exit
      iter <- 0 # Iteration number
      game.count <- 0 # counts the number of rounds played
      game.iter <- 0 # counts the number of iterations in a round
      tour <- sample(1:node.count)
      
# Distance Function
      
      dist <- function(vector){
        temp.dist <- 0 # Reset distance stored in memory
        for (k in 1:(node.count-1)){
          temp.dist <- sum(distance.matrix[tour[k],tour[k+1]], # Distance from first to second node
                           temp.dist) # Previous sum of distances
        } 
        temp.dist <- sum(distance.matrix[tour[node.count],tour[1]], # Distance from first to second node
                         temp.dist) # Previous sum of distances
        return(temp.dist)
      }
      
      
# Placement Function
      
      land.dist <- function(vector, scalar){
        temp.place <- rep(0, (node.count-game.players-1+i)) # Create an empty vector to store the differences
        for (j in 1:(node.count-game.players-2+i)){
          temp.place[j] <- sum(distance.matrix[jumpers[i],tour[j]], # New arc 2
                               distance.matrix[jumpers[i],tour[j+1]], # New arc 3
                               -distance.matrix[tour[j],tour[j+1]]) # Deleted arc 3
        }
        temp.place[(node.count-game.players-1+i)] <- sum(distance.matrix[jumpers[i],tour[node.count-1-game.players+i]], # New arc 2
                                        distance.matrix[jumpers[i],tour[1]], # New arc 3
                                        -distance.matrix[tour[node.count-1-game.players+i],tour[1]]) # Deleted arc 3
        return(temp.place)
      }
      
    #########################################################
    ############          MAIN BODY           ###############
    #########################################################

     
while (stop.crit == FALSE){
# Update stats
      iter <- iter + 1
      game.iter <- game.iter + 1
      
# Jump
      jumpers <- sample(2:node.count, size=game.players) # Choose the jumpers
      tour <- tour[!(tour %in% jumpers)]

# Land
      for (i in 1:game.players){
        land.values <- land.dist(tour, jumpers[i]) # Calculate the benefit from each landing spot
        if (game.iter < (land.high*game.length)){
          place.size <- ceiling(node.count*game.accuracy*(game.length-game.iter+1)/game.length) # Landing accuracy
        }else if(game.iter < ((land.med+land.high)*game.length)) {
            p <- runif(1)
            if (p < game.accuracy){
              place.size <- 2
            } else {
              place.size <- 1
            }
        } else {
          place.size <- 1
        }
          tour.place.rand <- sample(1:place.size)[1] # Pick the landing accuracy value
          tour.place <- order(land.values, decreasing=FALSE)[tour.place.rand] # Pick the landing spot
          
        if(tour.place < (node.count-game.players+i)){ # Place jumper[i] in the chosen landing spot
          tour <- c(tour[1:(tour.place)], jumpers[i], tour[(tour.place+1):(node.count-1-game.players+i)])  
        }else{
          tour <- c(tour,jumpers[i])  
        }
      }
      
# Recalculate
      tour.dist <- dist(tour)
      plot.dist <- rbind(plot.dist,c(iter,tour.dist,course.dist))
      
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
      print(c(iter,game.count,game.iter,tour.dist))
}

# Graph the distance vector  
  p <- plot_ly(plot.dist, x = ~iteration, y = ~distance, name = 'distance', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~optimal, name = 'optimal', mode = 'lines')
  print(p)
  
  print(c("optimality gap", best.dist, course.dist,best.dist/course.dist))
  