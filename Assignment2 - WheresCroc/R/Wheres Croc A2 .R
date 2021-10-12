library("WheresCroc")

# ------------------------------------------------------------------------------
# moveInfo
#   moves = 2 moves (to move to a hole / to check the hole)
#   mem  = store info turn to turn
#         status: (0 = first ever start),(1 = ongoing game),(2 = new game starts flag)
# readings 
#   salinity,phosphate and nitrogen values from our croc's sensor
# positions
#   Vector of positions of the 2 tourists and us
#   -1 if the tourist is eaten in this turn
#   NA if the tourist is eaten before.
# edges
# probs 
#   mean and standard deviation values of the waterholes
#   values for salinity, phosphate, nitrogen
# ------------------------------------------------------------------------------


# Steps
# 1) Initialize
#     - Ft_0
#     - Transition Matrix
# 2) Emission Matrix
# 
# 3) Find the new state Ft
#4) Check tourists (alive or not)
# 5) BFS
# 6) Return move


myFunction <- function(moveInfo,readings,positions,edges,probs) {
  
  # Define how many waterholes exists in the boad
  waterholes = 40
  
  # ------------------------------------------------------------------------------
  # INITIALIZATION
  # ---------Initialize Ft
  # Repeated at the beginning of each new game
  
  if(moveInfo$mem$status == 0) {
      #Matrix Transition
      transition_matrix = c() # We start with an empty matrix
      
      # 1/38 if both tourists are alive
      
      # Find reachable nodes from all nodes
      for (hole_number in 1:40){ 
          reachable_holes = getNeighbours(hole_number,edges)
          probability = 1/length(reachable_holes)
          
          new_row = matrix(0,1,40) # 1x40 zero vector
          
          for (i in 1:length(reachable_holes)){
            # For reachable holes, change the probability values
            new_row[1,reachable_holes[i]] = probability
          }
        
          # Append this row to our transition matrix 
          # We will have 40x40 matrix at the end
          transition_matrix =  rbind(transition_matrix,new_row)
          moveInfo$mem$tMatrix = transition_matrix
      }
      
      
      #Creation of the state vector with equal probabilities
      tourist1 = positions[[1]]
      tourist2 = positions[[2]]
    
      if(tourist1 < 0){
        FT_init = rep(0,waterholes)
        FT_init[tourist1] = 1
      } else if (tourist2 < 0){
        FT_init = rep(0,waterholes)
        FT_init[tourist2] = 1
      } else {
        FT_init = 1/(waterholes-2) * rep(1,waterholes)
        FT_init[tourist1] = 0
        FT_init[tourist2] = 0
      }
  

      #Memorize info like status and the state vector
      moveInfo$mem$status = -1
      moveInfo$mem$stateV  = FT_init
      
  }
  
  
  # Game continues dont need to do calculations
  else if(moveInfo$mem$status == -1){
      transition_matrix = moveInfo$mem$tMatrix 
  }
  
  
  # New gams started, recalculate the FT now
  else if(moveInfo$mem$status == 1){
    
    #Creation of the state vector with equal probabilities
    tourist1 = positions[[1]]
    tourist2 = positions[[2]]
    
    if(tourist1 < 0){
      FT_init = rep(0,waterholes)
      FT_init[tourist1] = 1
    } else if (tourist2 < 0){
      FT_init = rep(0,waterholes)
      FT_init[tourist2] = 1
    } else {
      FT_init = 1/(waterholes-2) * rep(1,waterholes)
      FT_init[tourist1] = 0
      FT_init[tourist2] = 0
    }

    #Memorize info like status and the stae vector
    moveInfo$mem$status = -1
    moveInfo$mem$stateV  = FT_init
  }
  

  # ------------------------------------------------------------------------------
  # EMISSION MATRIX
  salinity_list = dnorm(readings[1],probs$salinity[,1],probs$salinity[,2])
  phosphate_list =dnorm(readings[2],probs$phosphate[,1],probs$phosphate[,2])
  nitrogen_list = dnorm(readings[3],probs$nitrogen[,1],probs$nitrogen[,2])
  
  combined_list = normalize(salinity_list * phosphate_list * nitrogen_list)
  emission_matrix = diag(combined_list,waterholes,waterholes) 
  
  # ------------------------------------------------------------------------------
  # Calculating the new state (FT) and normalizing it

  FT_new = rep(0,40)
  
  # use alive tourist info
  tourist1 = positions[[1]]
  tourist2 = positions[[2]]
  
  shouldNormalize = FALSE
  
  if(tourist1 < 0 && !is.na(tourist1)) {
    FT_new[abs(tourist1)] = 1
  }else if(tourist2 < 0 && !is.na(tourist2)){
    FT_new[abs(tourist2)] = 1
  }else {
    FT_new = (moveInfo$mem$stateV %*% moveInfo$mem$tMatrix %*% emission_matrix)  # F = FxTxO
    FT_new[tourist1] = 0
    FT_new[tourist2] = 0
    FT_new = normalize(FT_new)
  }
  

  # ------------------------------------------------------------------------------
  # BFS
  current_location = positions[3]
  target_hole = which.max(FT_new)
  path = BFS(current_location,target_hole,edges)
  
  
  # ------------------------------------------------------------------------------
  # Set the moves
  if(length(path)==1){
    
    moveInfo$moves=c(path[[1]],0)
    FT_new[path[[1]]] = 0

  }
  
  else if(length(path)==2){
    moveInfo$moves=c(path[[2]],0)

  }
  else{
  moveInfo$moves=c(path[[2]],path[[3]])
  }
  
  # Update the state vector
  moveInfo$mem$stateV = FT_new
  return(moveInfo)
}



# ------------------------------------------------------------------------------
# BFS Algorithm
# ------------------------------------------------------------------------------

BFS <- function(start_node,target_node,edges){
  
  # Initialize the Frontier with the starting node
  Frontier = list(start_node)

  # Keep track of the visited nodes
  visited = matrix(FALSE,1,40) # 1x40 False vector
  visited[start_node] = TRUE   # Label the start node as visited
  
  prev = matrix(-1,1,40)  # Used to track the path
  
  while (length(Frontier) != 0){
    expanded = Frontier[[1]]  # Expand the first element from the Frontier (and remove it)
    Frontier = Frontier[-1]
       
    neighbours = getNeighbours(expanded,edges) # Find the neighbours of the expanded node
    
    for (i in 1:length(neighbours)){  # Check all neighbours
      
      if(prev[target_node] != -1){  # Check if we already reached to our target node
        break
      }
      
      if (visited[neighbours[[i]]] == FALSE){     # If this neighbour node is not visited before
        Frontier = append(Frontier,neighbours[i]) # Add this neighbour node to our Frontier
        visited[neighbours[[i]]] = TRUE           # Label it as visited
        prev[neighbours[[i]]] = expanded          # save how we reached this node
      }
    }
  }
  
  path = list()
  i = target_node

  # Use the prev list and track how we get to the target node
  while(i != -1){
    path = append(path,i)
    i = prev[i]
  }
  
  path = rev(path) # reverse the path since we found the path from target node to start node
  #print(target_node)
  return(path)
}



# Find the reachable holes from the current hole
getNeighbours=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

# normalize state vector 
normalize <- function(x){
  total = sum(x)
  return (x/total)
}