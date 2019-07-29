###############################################################
#2OPT
###############################################################
#General Idea Find best pair of edges (i,i+1) and (j,j+1) such that 
#replacing them with (i,j) and (i+1,j+1) minimizes tour length.
#In reality this is equivilent to running the portion of the 
#tour from i+1 to j backwards.  
#This is the key fact we leverage in this code.
#Rather then focusing on swapping edges then we just grab all permutations
#of tour and try running it in reverse order.


#Read in TSP Dataset named Lab_Data
library(readxl)
TSP_Data <- read_excel("I:/OPER 623/Spring 2018 (Cox)/Lecture 06  - Local Search Heuristics LAB/Lab Data.xlsx", col_names = FALSE, skip = 1)

#Turn it into a usable distance matrix
Data<-as.matrix(TSP_Data)

#Determine the number of cities in the data set
size<-dim(Data)[1]

#Intialize variables
tour = as.vector(1:size) #Initialize tour, could use NN, or random to get this, we just create an in order tour 
                        #tour stores order or nodes in tour, we can refernce this order by using R's index abilities
best_tour = tour #best_tour will hold the best tour seen so far, which we initalize with our starting tour
stop = 0 #stop hold the binary decision variable we will slip when stop criteria met
cost = 0 #cost holds the cost of the current approved tour, we will assign it the value of tour below
q=0
p=0
m=0

#calculate initial tour cost
k=1
for (k in 1:size-1){
  cost = sum(Data[tour[k],tour[k+1]] , cost);
} 
cost = sum(Data[tour[size],tour[1]] , cost)

#Initalize best cost seen so far
best_cost = cost

#An 2OPT edge swap directly corresponds to running the "in-between" portion of the tour backwards.
while (stop < 1){
  #outer i loop
  #one iteration loops thru all of i once and j "size" times
  #i.e for  i=1, j=2...n-1, for i=2, j=3...n-1,..., for i = n-2, j=n-1 
  i=0;  #Initalize outer loop
  while (i < size-1){   #While i=0...size-1 
    i<-i+1; #increment i
    j<-i+1; #initalize j, note this means inner loop is always starting at i+1
    
    #inner j loop
    while (j < size){ #While j=i+2...size 
      test_tour = replace(tour, i:j, rev(tour[i:j]));
      k=1;
      test_cost = 0;
      for (k in 1:size-1){
        test_cost = sum(Data[test_tour[k],test_tour[k+1]] , test_cost);
      } 
      test_cost = sum(Data[test_tour[size],tour[1]] , test_cost)
      p=p+1
      if (test_cost < best_cost) {q=q+1;
         best_tour = test_tour;
         best_cost = test_cost;
      }
    j<-j+1; #increment j
    }
  }

  #After inner loop and outer loop check to see if any improvement has been made,
  #if it has then change tour to be the best tour found within that iteration
  #if not stop heuristic we are at a local minima
  if(best_cost < cost) {
    print(m);
    m=m+1;
    tour = best_tour;
    cost = best_cost;}
  else{stop = 1;
  }
}



