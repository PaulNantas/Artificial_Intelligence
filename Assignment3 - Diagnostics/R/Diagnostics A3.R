
# AI HOMEWORK 3 / 2021 Fall
# Mustafa Gokturk Yandim & Paul Nantas

#library("Diagnostics")

# Pneumenia = Pn
# Temperature = Te
# Tuberculosis = TB
# Visited TB Spot = VTB
# Smokes = Sm
# Cancer = LC
# Bronchitis = Br
# X-Ray = XR
# Dyspnea = Dy


# ----------------------------------------------------------------------
# Your function which will create the network.
# It should accept as a single argument the historical cases as 
# returned by the Get_Historical_Data function.

learn <- function(historical_data) {
  
  # Get the total data (column) size of the training dara
  data_size = length(historical_data[,1])

  # ************************
  # Calculate probabilities for Pneumenia = Pn (1x2 matrix)
  Pn_true_count = length(which(historical_data$Pn==1))
  Pn_false_count = length(which(historical_data$Pn==0))
  
 
  Pn = matrix(normalize(c(Pn_false_count,Pn_true_count)),nrow=1,ncol=2,byrow = TRUE) 
  dimnames(Pn) = list(c(1),c("false", "true")) # Give names to the Pn matrix
  
  
  # ************************
  # Calculate probabilities for Visited TB Spot = VTB (1x2 matrix)
  VTB_true_count = length(which(historical_data$VTB==1))
  VTB_false_count = length(which(historical_data$VTB==0))

  
  VTB = matrix(normalize(c(VTB_false_count,VTB_true_count)),nrow=1,ncol=2,byrow = TRUE) 
  dimnames(VTB) = list(c(1),c("false", "true")) 
  
  # ************************
  # Calculate probabilities for Smokes = Sm (1x2 matrix)
  Sm_true_count = length(which(historical_data$Sm==1))
  Sm_false_count = length(which(historical_data$Sm==0))
  
  Sm = matrix(normalize(c(Sm_false_count,Sm_true_count)),nrow=1,ncol=2,byrow = TRUE) 
  dimnames(Sm) = list(c(1),c("false", "true")) 
  
  
  # ************************
  # Calculate probabilities for Tuberculosis = TB (2x2 matrix)
  
  TB = matrix(
    c(nrow(historical_data[which(historical_data$VTB == 0 & historical_data$TB == 0),]),
      nrow(historical_data[which(historical_data$VTB == 0 & historical_data$TB == 1),]),
      nrow(historical_data[which(historical_data$VTB == 1 & historical_data$TB == 0),]),
      nrow(historical_data[which(historical_data$VTB == 1 & historical_data$TB == 1),])),
      nrow=2,ncol=2, byrow = TRUE)
  dimnames(TB) = list(c(1,2),c("false", "true")) 

  # Normalize each row 
  for (i in 1:length(TB[,1])){
    TB[i,] = normalize(TB[i,])
  }
  
  # ************************
  # Calculate probabilities for Cancer = LC (2x2 matrix)
  
  LC = matrix(
    c(nrow(historical_data[which(historical_data$Sm == 0 & historical_data$LC == 0),]),
      nrow(historical_data[which(historical_data$Sm == 0 & historical_data$LC == 1),]),
      nrow(historical_data[which(historical_data$Sm == 1 & historical_data$LC == 0),]),
      nrow(historical_data[which(historical_data$Sm == 1 & historical_data$LC == 1),])),
      nrow=2,ncol=2, byrow = TRUE)
  dimnames(LC) = list(c(1,2),c("false", "true")) 
  
  # Normalize each row 
  for (i in 1:length(LC[,1])){
    LC[i,] = normalize(LC[i,])
  }
  
  # ************************
  # Calculate probabilities for Bronchitis = Br (2x2 matrix)
  
  Br = matrix(
    c(nrow(historical_data[which(historical_data$Sm == 0 & historical_data$Br == 0),]),
      nrow(historical_data[which(historical_data$Sm == 0 & historical_data$Br == 1),]),
      nrow(historical_data[which(historical_data$Sm == 1 & historical_data$Br == 0),]),
      nrow(historical_data[which(historical_data$Sm == 1 & historical_data$Br == 1),])),
    nrow=2,ncol=2, byrow = TRUE)
  dimnames(Br) = list(c(1, 2),c("false", "true")) 
  
  # Normalize each row 
  for (i in 1:length(Br[,1])){
    Br[i,] = normalize(Br[i,])
  }
  
  # ************************
  # Calculate probabilities for Dyspnea = Dy (4x2 matrix)
  
  Dy = matrix(
    c(nrow(historical_data[which(historical_data$LC == 0 & historical_data$Br == 0 & historical_data$Dy == 0),]),
      nrow(historical_data[which(historical_data$LC == 0 & historical_data$Br == 0 & historical_data$Dy == 1),]),
      nrow(historical_data[which(historical_data$LC == 0 & historical_data$Br == 1 & historical_data$Dy == 0),]),
      nrow(historical_data[which(historical_data$LC == 0 & historical_data$Br == 1 & historical_data$Dy == 1),]),
      nrow(historical_data[which(historical_data$LC == 1 & historical_data$Br == 0 & historical_data$Dy == 0),]),
      nrow(historical_data[which(historical_data$LC == 1 & historical_data$Br == 0 & historical_data$Dy == 1),]),
      nrow(historical_data[which(historical_data$LC == 1 & historical_data$Br == 1 & historical_data$Dy == 0),]),
      nrow(historical_data[which(historical_data$LC == 1 & historical_data$Br == 1 & historical_data$Dy == 1),])),
    nrow=4,ncol=2, byrow = TRUE)
  dimnames(Dy) = list(c(1:4),c("false","true")) 
  
  # Normalize each row 
  for (i in 1:length(Dy[,1])){
    Dy[i,] = normalize(Dy[i,])
  }
  
  
  # ************************
  # Calculate probabilities for X-Ray = XR (8x2 matrix)
  
  XR = matrix(
    c(nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 0 & historical_data$LC == 0 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 0 & historical_data$LC == 0 & historical_data$XR == 1),]),
      
      nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 0 & historical_data$LC == 1 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 0 & historical_data$LC == 1 & historical_data$XR == 1),]),
      
      nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 1 & historical_data$LC == 0 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 1 & historical_data$LC == 0 & historical_data$XR == 1),]),
      
      nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 1 & historical_data$LC == 1 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 0 & historical_data$TB == 1 & historical_data$LC == 1 & historical_data$XR == 1),]),
      
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 0 & historical_data$LC == 0 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 0 & historical_data$LC == 0 & historical_data$XR == 1),]),
      
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 0 & historical_data$LC == 1 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 0 & historical_data$LC == 1 & historical_data$XR == 1),]),
      
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 1 & historical_data$LC == 0 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 1 & historical_data$LC == 0 & historical_data$XR == 1),]),
      
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 1 & historical_data$LC == 1 & historical_data$XR == 0),]),
      nrow(historical_data[which(historical_data$Pn == 1 & historical_data$TB == 1 & historical_data$LC == 1 & historical_data$XR == 1),])),
    nrow=8,ncol=2, byrow = TRUE)
  dimnames(XR) = list(c(1:8),c("false","true"))
  
  # Normalize each row 
  for (i in 1:length(XR[,1])){
    XR[i,] = normalize(XR[i,])
  }

#Temperature 
  mean_Pn0 = mean(historical_data[historical_data$Pn==0,"Te"])
  mean_Pn1 = mean(historical_data[historical_data$Pn==1,"Te"])
  std_Pn0 = sd(historical_data[historical_data$Pn==0,"Te"])
  std_Pn1 = sd(historical_data[historical_data$Pn==1,"Te"])
  
  Te = matrix(c(mean_Pn0,mean_Pn1,std_Pn0,std_Pn1),nrow=2,ncol=2,byrow = TRUE)
              
  dimnames(Te) = list(c('Mean','Std'),c('Pn=0','Pn=1'))
  
  
  network = list(Te = Te,Pn = Pn,VTB = VTB,Sm = Sm,TB = TB,LC = LC,Br = Br,Dy = Dy,XR = XR)
  
  # Return the network as a list
  return(network)
}




# ----------------------------------------------------------------------
# Your function which will use the network created by your 
# learn function to estimate probabilities of unknown variables
# in a set of cases. This function should take two arguments: 
# (1) your network, as returned by your learn function; 
# and (2) The cases, as returned by the Get_Cases function.

#Steps
#1 : We have to determine each "path" to get the probability (chain rule)
#2 : Assigned random values to unknown values (Pn, Tb, Lc, Br)
#3 : We calculate p_old
#4 : We assign a new value to an unknown value : we calculate p_new
#5  : We compare p_old to p_new : if p_new > p_old we accept the new value of the unknown parameter
#6 : If p_new<p_old, we accept a new value with a probability p=p_new/p_old, we compare it to a number choose from a uniform variable [0,1]
#7 : Run it 1000 times for each 10 cases
#8 : Average the value assigned to disease and choose our prediction.

# Pneumenia = Pn
# Temperature = Te
# Tuberculosis = TB
# Visited TB Spot = VTB
# Smokes = Sm
# Cancer = LC
# Bronchitis = Br
# X-Ray = XR
# Dyspnea = Dy

diagnose <- function(network,cases) {

  decisions = matrix(0,10,4)                        # Calculated results
  dimnames(decisions) = list(c(1:10),c('Pn','TB','LC','Br'))
  
  
  for (i in 1:length(cases[,1])){                   # Loop for all test cases
    
    unif_dist = runif(1000,min=0,max=1)
    assigned = cases[i,]                            # Retrieve the test case data (current row)
    unknown_diseases =  which(is.na(cases[i,]))     # Store the unknown diseases indexes
    assigned[is.na(assigned)] = random()            # Assign random values (1 or 0) to NA data

    case_samples = list()
    
    for (j in 1:1000){                              # Sample 1000 times
      for (unknown in 1:length(unknown_diseases)){    # Change one of the unknown/NA disease value
        
        # Calculate the old probability
        p_old = chain(network,assigned)
        
        # Create a new proposed values using the candidate function
        proposed = assigned
        proposed[[unknown_diseases[unknown]]] = candidate_func(assigned[[unknown_diseases[unknown]]])  
        
        # Calculate the new probability
        p_new = chain(network,proposed)
        
        # Compare p_new and p_old values
        if(p_new > p_old){
          assigned[[unknown_diseases[unknown]]] = proposed[[unknown_diseases[unknown]]]   
          # We accept the proposed value since it is larger
        } else {
          
          # Check if we should accept the new proposed value using change_assigned() function
          if(change_assigned(p_new,p_old)){
          #if((p_new/p_old) > unif_dist[j]){
            assigned[[unknown_diseases[unknown]]] = proposed[[unknown_diseases[unknown]]]  
            # We accept the proposed value 
          }
        }
        
        
      } # End of for loop, now do the same for other unknown diseases
      
      # Add this new sample to our samples list
      if (j > 200){ # Burn first 200 samples
        case_samples = rbind(case_samples, assigned)
      }

    } # We finished 1000 samples
    
    #print(sum(case_samples$Pn)/nrow(case_samples))
    
    # Calculate the mean values for each disease in sample to determine the final diagnostic
    decisions[,"Pn"][i] = sum(case_samples$Pn)/nrow(case_samples)
    decisions[,"TB"][i] = sum(case_samples$TB)/nrow(case_samples)
    decisions[,"LC"][i] = sum(case_samples$LC)/nrow(case_samples)
    decisions[,"Br"][i] = sum(case_samples$Br)/nrow(case_samples)

  } # For loop for a case has finished
  
  return(decisions)
}


# HELPER FUNCTIONS
# --------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------
# Apply chain rule to calculate the Probability
# P(Pn).P(VTB).P(Sm). P(TB|VTB).P(LC|Sm).P(Br|Sm) .P(Dy|LC,Br).  P(XR|Pn,TB,LC)  P(Te|Pn)

chain <- function(network,diseases){
  
  # network $ DISEASE_NAME [row] [column]
  
  #  P(Te |Pn)
  temperature = dnorm(diseases$Te,network$Te[1,diseases$Pn+1],network$Te[2,diseases$Pn+1])

  # P(Pn).P(VTB).P(Sm)
  part1 = (network$Pn[1,diseases$Pn + 1]) * (network$VTB[1,diseases$VTB + 1]) * (network$Sm[1,diseases$Sm + 1])
  
  # P(TB|VTB).P(LC|Sm).P(Br|Sm)
  part2 = (network$TB[diseases$VTB + 1,diseases$TB + 1])*(network$LC[diseases$Sm + 1,diseases$LC + 1])*(network$Br[diseases$Sm + 1,diseases$Br + 1])
 
  # P(Dy|LC,Br)
  part3 = (network$Dy[bitsToInt(c(diseases$LC,diseases$Br))+1, diseases$Dy + 1])
  
  # P(XR|Pn,TB,LC)
  part4 = (network$XR[bitsToInt(c(diseases$Pn,diseases$TB,diseases$LC))+1, diseases$XR + 1])
  
  total = part1*part2*part3*part4*temperature
  
  return(total)
}


# ----------------------------------------------------------------------
# Convert bianry to integer


bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}


# ----------------------------------------------------------------------
# Candidate function
# f(x) = 0 if x=1 , and f(x) = 1 otherwise

candidate_func <- function(value){
  if (value == 1){
    return(0)
  } else {
    return(1)
  }
}


# ----------------------------------------------------------------------
# Calculates the probability p_new/p_old and returns TRUE or FALSE

change_assigned <- function(new,old){
  
  prob = new/old
  random = runif(1)  # Random number between 0-1
  
  if (random <= prob){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

# ----------------------------------------------------------------------
# Return a random value of 1 or 0

random <- function(){
  return(sample(c(0,1),1))
}

# ----------------------------------------------------------------------
# normalize state vector 
normalize <- function(x){
  total = sum(x)
  return (x/total)
}