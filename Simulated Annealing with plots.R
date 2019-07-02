########## Simulated Annealing ################
time<-proc.time()
graphvector<-matrix()
#Read in Dataset
setwd("~/Documents/Brandon/AFIT/2019_Q2/OPER_623 Heuristic Search Methods/Lab_Week05")
library(readxl)  #Need to install the "readxl" package!
TSP_Data <- read_excel("TSP Data.xlsx", sheet = "gr120", col_names = FALSE)
distance.matrix<-as.matrix(TSP_Data)
load("~/Documents/Brandon/AFIT/2019_Q2/OPER_623 Heuristic Search Methods/Lab_Week05/gr120_nn.RData")

number.nodes<-dim(TSP_Data)[1]
originalTour <- c(sample(1:number.nodes)) # Create the intial tour
original.distance <- 0
#calculate initial tour distance
for (k in 1:number.nodes-1){
  original.distance <- sum(distance.matrix[originalTour[k],originalTour[k+1]] , original.distance);
} 
original.distance <- sum(distance.matrix[originalTour[number.nodes],originalTour[1]] , original.distance)

#initialize parameters
temperature<-1000
iterations<-500
alpha<-0.85
counter<-0
StoppingCrit<-F
counter.tol<-10000
current.Tour<-originalTour
current.distance<-original.distance
best.Tour<-current.Tour
best.distance<-current.distance
graphvector<-c(original.distance)
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
    for (k in 1:number.nodes-1){
      test.distance <- sum(distance.matrix[test.Tour[k],test.Tour[k+1]] , test.distance);
    } 
    test.distance <- sum(distance.matrix[test.Tour[number.nodes],test.Tour[1]] , test.distance)
    
    
    delta<-test.distance-current.distance
    if (delta<0){
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
      } else{counter<-(counter+1)}
    }
      if(best.distance>current.distance){
    best.distance<-current.distance
    best.Tour<-current.Tour
    graphvectorbest<-c(graphvectorbest, current.distance)
  }
  }
  if (counter>counter.tol){StoppingCrit<-T}
  temperature <- temperature * alpha
  print(c(temperature, counter, best.distance))
}

print(best.distance)
#print(proc.time()-time)
print(best.Tour)
plot(graphvector)
