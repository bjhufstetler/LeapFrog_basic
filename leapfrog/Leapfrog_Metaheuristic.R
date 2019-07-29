  #########################################################
  ############             SETUP            ###############
  #########################################################
  
  # Clean the environment, and console
    rm(list = ls()) 
    cat("\f")
    
  # Setup
    library(readxl)
    setwd("~/Documents/Brandon/AFIT/2019_Q2/OPER_623 Heuristic Search Methods/Project/leapfrog")
    load("course_list.RData")

  #########################################################
  ############      TUNABLE PARAMETERS      ###############
  #########################################################

  # (1) eil51   (2) ts225     (3) pr1002   (4) gr120    (5) rat195,
  # (6) Bays29  (7) Berlin52  (8) Cho130   (9) KroA100  (10) pcb442,
  # (11) pr76   (12) gr48     (13) pma343
    
    game.map <- 7 # Pick which map to use [1,13]
    game.max <- 10 # Total number of games played [1,inf)
    game.length <- 1000 # Iterations in each round [1,inf)
    
    game.players <- 1 # Number of players (0,1]
    game.accuracy <- 0.5 # Starting accuracy of the players (0,1]
    game.alpha <- 100 # Player reduction curve [0,game.length]
    
  #########################################################
  ############        INITIALIZATION        ###############
  #########################################################
      
    course.name <- course.list$name[game.map] # Use game.map to get the map name string
    course.dist <- course.list$distance[game.map] # Use game.map to get the map optimal distance
  # Import the internodal distance information as a matrix
    dist.matrix <- as.matrix(read_excel("TSP Data.xlsx", sheet = course.name, col_names = FALSE))
    node.count <- dim(dist.matrix)[1] # Total nodes on map
    
    stop.crit <- FALSE # Set to true in main body when ready to exit
    game.count <- 0 # Counts the number of games played
    game.iter <- 0 # Counts the number of rounds played
    tour <- sample(1:node.count) # Create a random starting tour
    
  #########################################################
  ############      DISTANCE FUNCTION       ###############
  #########################################################
      
  # Copy the first node to the end of the tour, then take each node
  # in order as a combination of n, n+1. Use that combo to look up
  # the cost in the distance matrix. Sum all costs.
    dist = function(tour, dist.matrix) { 
      sum(dist.matrix[embed(c(tour, tour[1]), 2)]) 
    }
    tour.dist <- dist(tour, dist.matrix) # Calculate the starting tour length
    best.dist <- tour.dist # Set the starting distance as the best so far
    
  #########################################################
  ############    LAND DISTANCE FUNCTION    ###############
  #########################################################
  
  # Calculate and store the cost difference for placing a node at each available location in a tour
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
    
    time <- proc.time() # Start a timer
    
    while (stop.crit == FALSE){
  # Update round number
          game.iter <- game.iter + 1
          
  # Calculate p' using the decay function
          p <- max(ceiling(game.players * (node.count - 3) * exp(-game.alpha*(game.iter-1)/game.length)),1)
         
  # Jump
          jumpers <- sample(1:node.count, size=ceiling(p)) # Choose the jumpers at random
          tour <- tour[!(tour %in% jumpers)] # Remove the jumpers from the tour
    
  # Land
          for (i in 1:ceiling(p)){
            land.values <- land.dist(tour, jumpers[i]) # Calculate the differential vector
          # On the first round of every game, choose the landing spot based on game.accuracy
            if(game.iter==1 & game.count>0){
              place.size <- max((node.count-((node.count-3)*game.players))*game.accuracy,1) # Landing accuracy
            } else {
              place.size <- 1 # Land in the best spot possible
            }
              tour.place.rand <- sample(1:place.size)[1] # Pick the landing node rank
              tour.place <- order(land.values, decreasing=FALSE)[tour.place.rand] # Assign the landing spot
          # Place the jumper in the chosing landing spot  
            if(tour.place < length(land.values)){
              tour <- c(tour[1:(tour.place)], jumpers[i], tour[(tour.place+1):(node.count-1-ceiling(p)+i)])  
            }else{
              tour <- c(tour,jumpers[i])  
            }
          }
          
    # Recalculate distance
          tour.dist <- dist(tour, dist.matrix) # Get the tour distance
          
          if (tour.dist < best.dist){ # If new tour beats best so far, update best so far
            best.dist <- tour.dist
            tour.best <- tour
          }
          
    # Check round length and game length
          if (game.iter==game.length){
            game.iter <- 0 # Start a new game
            game.count <- game.count + 1 # Update the game counter
          }
          
          if (game.count == game.max){
            stop.crit <- TRUE # Exit when the max number of games has been played
          }
    }

    time <- proc.time() - time # Stop the timer

  # Output the results in an easily digestible format
    output <- as.data.frame(matrix(c(course.name, course.dist, best.dist, round(100*(best.dist-course.dist)/course.dist,3), round(time[1],3)), byrow=TRUE, nrow=1))
    names(output) <- c("map", "optimal", "LF", "Error", "Runtime")
    print(output)
