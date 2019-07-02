######################################################################
#                  Simulated Annealing (SA) Lab Code
#                        Lt Col Bruce Cox
# This code provides a basic, hopefully informative, implementation 
# implementation of the SA heuristicfor the Traveling Salesman Problem.  
#######################################################################

#User Defined Parameters
number_of_iterations <- 50000
temp <- 5500
Beta <-.000001

#Read in TSP Dataset named Lab_Data, this dataset is a symmatric nxn matrix of distances
library(readxl)
TSP_Data <- read_excel("I:/OPER 623/Spring 2018 (Cox)/Lecture 06  - Local Search Heuristics LAB/Lab Data.xlsx", col_names = FALSE, skip = 1)


#Turn dataset into a usable distance matrix
distance_matrix<-as.matrix(TSP_Data)


#calculate number of Cities in instance (dim produces a 1x2 vector (# rows, #columns). I only need #rows since this is symmetric)
n_cities<-dim(distance_matrix)[1]


#Define function for calculating tour distance
   # How?  c(tour, tour[1]) addes the first node of the tour back onto the end of the tour A-> B-> C-> A
   # embed(full_tour, 2) takes this full tour and breaks it up into 1x2 vectors (A,B), (B,C), (C,A)
   # Distance_matrix[m,n] outputs the value of distance matrix at row m and column n for all of the above 1x2 vectors
   # sum then adds all these distances together. Elegent!
calculate_tour_distance = function(tour, distance_matrix) { 
   sum(distance_matrix[embed(c(tour, tour[1]), 2)]) 
}

# Create an initial tour and calculate its distance
tour = as.vector(1:n_cities)
tour_distance = calculate_tour_distance(tour, distance_matrix)

#Set Best tour and best distance equal to tour - since this is the only tour known so far its the best tour
best_tour=tour
best_distance = calculate_tour_distance(best_tour, distance_matrix)

####################################
# Main Simulated Annealing code ####
####################################
   for(i in 1:number_of_iterations) { 
     iter =  i 
     
     #update temperature with Lundy and Mess calculation
     temp = temp/(1+Beta*temp)
     
     candidate_tour = tour #clone tour and manipulate it's clone
     swap = sample(n_cities, 2) #this randomly grabs 2 cities swap1 and swap2 from the tour
     candidate_tour[swap[1]:swap[2]] = rev(candidate_tour[swap[1]:swap[2]]) #2OPT: Replace the section of tour between swap1 and swap2 with its reverse  
     candidate_dist = calculate_tour_distance(candidate_tour, distance_matrix) #call distance function to calculate new distance
 
     #If candidate tour is worse then current tour create the probability value we will accept it at, if it is better accept it
     if (tour_distance < candidate_dist) { 
       ratio = exp((tour_distance - candidate_dist) / temp) #if candidate tour is worse then current tour create the probability value we will accept it anyway
     } else { 
       ratio = as.numeric(candidate_dist < tour_distance) #logical test, if candidate tour < current tour then ratio = 1 (i.e probability acceptance = 1)  
     } 
     
     #draw uniform random number, and accept candidate with probability as calculated above
     if (runif(1) < ratio) { #runif(1) draws a random number between 0 and 1 from a uniform distribution
       tour = candidate_tour 
       tour_distance = candidate_dist 
        
       if (tour_distance < best_distance) { #If new tour is better then best tour seen so far 
         best_tour = tour                   #update best so far tour order and,
         best_distance = tour_distance      #update best so far cost
       } 
     } 
   } 
  