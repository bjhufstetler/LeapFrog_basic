# Clean the environment, and console
  rm(list = ls()) 
  cat("\f")
  # eil51, ts225, pr1002, gr120, rat195, Bays29, Berlin52, Cho130, KroA100, pcb442, pr76, gr48, pma343
  
# Setup
  library(readxl)
  library(plotly)
  setwd("~/Documents/Brandon/AFIT/2019_Q2/OPER_623 Heuristic Search Methods/Project/leapfrog")
  load("course_list.RData")
  load("coords.RData")

    #########################################################
    ############      TUNABLE PARAMETERS      ###############
    #########################################################

      game.map <- 6 # Pick which map to use [1,13]
      game.max <- 10 # Total number of games played
      game.settings.recommended <- FALSE # Defaults to recommended settings based on node count
      
      game.players <- 1 # Number of players [1, number of nodes]
      game.accuracy <- .5 # Starting accuracy of the players (0,1]
      game.length <- 100 # Iterations in each round
      game.players.loss <- TRUE # dynamic player count
      game.players.loss.rate <- 1 # percent fewer players each round
      
      
      
# Initialization
      course.name <- course.list$name[game.map]
      course.dist <- course.list$distance[game.map]
      course.coords <- course.coords[course.coords$Course == course.name,]
      distance.matrix <- as.matrix(read_excel("TSP Data.xlsx", sheet = course.name, col_names = FALSE))
      node.count <- dim(distance.matrix)[1] # Total nodes on map
      best.dist <- 10^10 # Set higher than any random tour would create
      
        
      stop.crit <- FALSE # Set to true in main body when ready to exit
      iter <- 0 # Iteration number
      game.count <- 0 # counts the number of rounds played
      game.iter <- 0 # counts the number of iterations in a round
      tour <- sample(1:node.count)
      tour.x <- course.coords$X[match(tour,course.coords$Node)]
      tour.y <- course.coords$Y[match(tour,course.coords$Node)]
      plot.coords <- cbind(rep(1,node.count),tour,tour.x,tour.y)
      plot.coords <- rbind(plot.coords,plot.coords[1,])
      
      if(game.settings.recommended == TRUE){
        game.players <- 0.75
        game.accuracy <- 0.5
        game.length <- round(node.count/2.5)
        game.players.loss <- TRUE # dynamic player count
        game.players.loss.rate <- 2 # percent fewer players each round
      }
      
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
      plot.dist <- data.frame(0,dist(tour),course.dist) # Used to track the tour lengths over time
        names(plot.dist) <- c("iteration", "distance", "optimal")
# Placement Function
      
      land.dist <- function(vector, scalar){
        temp.place <- rep(0, (node.count-ceiling(p)-1+i)) # Create an empty vector to store the differences
        for (j in 1:(node.count-ceiling(p)-2+i)){
          temp.place[j] <- sum(distance.matrix[jumpers[i],tour[j]], # New arc 2
                               distance.matrix[jumpers[i],tour[j+1]], # New arc 3
                               -distance.matrix[tour[j],tour[j+1]]) # Deleted arc 3
        }
        temp.place[(node.count-ceiling(p)-1+i)] <- sum(distance.matrix[jumpers[i],tour[node.count-1-ceiling(p)+i]], # New arc 2
                                        distance.matrix[jumpers[i],tour[1]], # New arc 3
                                        -distance.matrix[tour[node.count-1-ceiling(p)+i],tour[1]]) # Deleted arc 3
        return(temp.place)
      }
      
    #########################################################
    ############          MAIN BODY           ###############
    #########################################################

 
while (stop.crit == FALSE){
# Update stats
      iter <- iter + 1
      game.iter <- game.iter + 1
      if (game.iter == 1){
        p <- game.players*(node.count-4)
      } else {
        if(game.players.loss == TRUE){
          p <- (p-(game.players*(node.count-4)/(game.length*game.players.loss.rate)))
        } else {
          p <- game.players*(node.count-4)
        }
      }
     
# Jump
      jumpers <- sample(1:node.count, size=ceiling(p)) # Choose the jumpers
      tour <- tour[!(tour %in% jumpers)]

# Land
      for (i in 1:ceiling(p)){
        land.values <- land.dist(tour, jumpers[i]) # Calculate the benefit from each landing spot
        if(game.iter==1){
          place.size <- round((node.count-((node.count-4)*game.players))*game.accuracy) # Landing accuracy
        } else {
          place.size <- 1 # Land in the best spot possible
        }
          tour.place.rand <- sample(1:place.size)[1] # Pick the landing accuracy value
          tour.place <- order(land.values, decreasing=FALSE)[tour.place.rand] # Pick the landing spot
          
        if(tour.place < (node.count-ceiling(p)+i)){ # Place jumper[i] in the chosen landing spot
          tour <- c(tour[1:(tour.place)], jumpers[i], tour[(tour.place+1):(node.count-1-ceiling(p)+i)])  
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
      if (sum(is.na(tour))==0){
      tour.x <- course.coords$X[match(tour,course.coords$Node)]
      tour.y <- course.coords$Y[match(tour,course.coords$Node)]
      plot.coords.new <- cbind(rep((iter+1),node.count),tour,tour.x,tour.y)
      plot.coords <- rbind(plot.coords,plot.coords.new)
      plot.coords <- rbind(plot.coords,plot.coords[(dim(plot.coords)[1]-node.count+1),])
      print(c(iter,game.count,game.iter,round(p),tour.dist))
      }
}
     
# Graph the distance vector  
  plot.graph <- plot_ly(plot.dist, x = ~iteration, y = ~distance, name = 'distance', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~optimal, name = 'optimal', mode = 'lines')
  print(plot.graph)
  
  

  plot.coords <- as.data.frame(plot.coords)
  plot.coords.graph <- plot_ly(plot.coords, x = ~tour.x, y = ~tour.y, name = 'location', type = 'scatter', mode = 'lines',
                               frame = ~V1, text = ~tour, textposition = 'middle right',textfont = list(color = '#000000', size = 16)) %>%
    layout(
      title = course.name,
      yaxis = list(
        title = "Y coord", 
        zeroline = F,
        showgrid = F
      ),
      xaxis = list(
        title = "X coord", 
        zeroline = F, 
        showgrid = F
      )
    ) %>% 
    animation_opts(
      frame = 50, 
      transition = 1, 
      redraw = FALSE
    )
  print(plot.coords.graph)

  print(c("optimality gap", best.dist, course.dist,abs(course.dist-best.dist)/course.dist))