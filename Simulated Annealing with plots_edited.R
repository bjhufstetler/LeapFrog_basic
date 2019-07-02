########## Simulated Annealing ################
graphvector<-matrix()
#Read in Dataset
library(readxl)  #Need to install the "readxl" package!
setwd("~/Documents/Brandon/AFIT/2019_Q2/OPER_623 Heuristic Search Methods/Lab_Week05")
TSP_Data <- read_excel("TSP Data.xlsx", sheet = "gr120", col_names = FALSE) # 6942=shortest path
#TSP_Data <- read_excel("TSP Data.xlsx", sheet = "Cho130", col_names = FALSE)
load("~/Documents/Brandon/AFIT/2019_Q2/OPER_623 Heuristic Search Methods/Lab_Week05/gr120_nn.RData")

distance.matrix<-as.matrix(TSP_Data)

number.nodes<-dim(TSP_Data)[1]
#originalTour <- c(sample(1:number.nodes)) # Create the intial tour

#calculate initial tour distance
#original.distance <- 0
#for (k in 1:(number.nodes-1)){
#  original.distance <- sum(distance.matrix[originalTour[k],originalTour[k+1]] , original.distance);
#} 
#original.distance <- sum(distance.matrix[originalTour[number.nodes],originalTour[1]] , original.distance)

#initialize parameters
iterations<-500
counter.tol<-10000
#graphvector<-c(original.distance)
#graphvectorbest<-c(best.distance)
alpha_c = as.data.frame(t(rep(0,3)))
names(alpha_c)=c("Start","Best Cost","Time")
m=0
  for (seed in 1:number.nodes){
  m = m+1
  originalTour <- as.integer(genes.best[seed,1:number.nodes]) # Create the intial tour
  original.distance <- as.integer(genes.best[seed,number.nodes+1])

  time<-proc.time()
  alpha = .9
  temperature<-100
  counter<-0
  StoppingCrit<-F
  current.Tour<-originalTour
  current.distance<-original.distance
  best.Tour<-current.Tour
  best.distance<-current.distance
  graphvectorbest<-c(best.distance)
    while (StoppingCrit==F) {
      #print("Current Tour and Distance")
      #print(current.Tour)
      #print(getTotalDistance(current.Tour, distance.matrix))
      #print(temperature)
      
      for(s in 1:iterations){
        #Randomly select N from a subset of S
        random<-sample(2:number.nodes,2,replace = F)
        i<-random[1]
        j<-random[2]
        
        test.Tour <- replace(current.Tour, i:j, rev(current.Tour[i:j]));
        #calculate  tour current.distance
        test.distance<-0
        for (k in 1:(number.nodes-1)){
          test.distance <- sum(distance.matrix[as.numeric(test.Tour[k]),as.numeric(test.Tour[k+1])] , test.distance);
        } 
        test.distance <- sum(distance.matrix[as.numeric(test.Tour[number.nodes]),as.numeric(test.Tour[1])] , test.distance)
    
        delta<-test.distance-current.distance
        if (delta<0){ # New better solution (always accept)
          current.distance <- test.distance #If so, assign current.distance  = distance_iter
          current.Tour <- test.Tour #Assign the new tour
          graphvector<-c(graphvector,current.distance)
          counter<-0
        } else{
          prob<-exp(-delta/temperature)
          p<-runif(1)
          if (p<prob) {
            current.distance <- test.distance #If so, assign current.distance  = distance_iter
            current.Tour <- test.Tour #Assign the new tour
            graphvector<-c(graphvector,current.distance)
            counter<-0
          } else{counter<-counter+1}
        }
          if(best.distance>current.distance){
            best.distance<-current.distance
            best.Tour<-current.Tour
            graphvectorbest<-c(graphvectorbest, current.distance)
          }
      }
      if (counter>counter.tol){
        StoppingCrit<-T
      }
      temperature <- temperature * alpha
      print(c(seed,temperature, counter, best.distance))
    }
  alpha_c[m,]=c(seed,best.distance,(time-proc.time())[3])
}

print(best.distance)
#print(proc.time()-time)
print(best.Tour)
plot(graphvector)
