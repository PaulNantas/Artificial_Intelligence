# Load the library
library("DeliveryMan")


#----------------------------------------------------------------
# Main Function to provide into the runDeliveryMan function

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {

  # Looking for the next package
  if(carInfo$load == 0) {
    carInfo$mem$goal <- nextPickup(trafficMatrix, 
                                   carInfo, 
                                   packageMatrix)
  } else { # Already have a package to deliver
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  
  # How do we get there?
  carInfo$nextMove <- A_Search(list(),trafficMatrix, list(carInfo$x,carInfo$y) , carInfo$mem$goal)
  
  return(carInfo)
}

#----------------------------------------------------------------
# Find the nearest pickup location for an undelivered package (Manhattan Distance)
# returns coordinates as in the form (x y)

nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  
  # Calculate Manhattan distances to the all packages in the map
  # Manhattan Distance = |x1-x2| + |y1-y2|
  manhattanDistanceVector = abs((packageMatrix[,1] - carInfo$x)) +
    abs((packageMatrix[,2] - carInfo$y))
  
  # Label the distance as inf for the packages that are  already picked or delivered 
  manhattanDistanceVector[packageMatrix[,5] != 0] = Inf
  
  return(packageMatrix[which.min(manhattanDistanceVector), c(1,2)])
}


#----------------------------------------------------------------
# A* Search Algorithm

A_Search <- function(Frontier,trafficMatrix, currentNode, goalNode)  {
  
  # Initial case
  if (length(Frontier) == 0){
    
    init = list(coord  = currentNode,g = 0,h = 0, path = list(currentNode))
    Frontier = append(Frontier,list(init))
    index = 1
    
    expanded = Frontier[[index]]
    expanded_coord = list(expanded$coord[[1]],expanded$coord[[2]])
    expanded_path = list(expanded_coord)

    
  } else {
    
    # find the minimum cost node
    f_values = lapply(Frontier, function(item) item$g + item$h)
    index = which.min(f_values)
    
    currentNode = Frontier[[index]]$coord
    
    expanded = Frontier[[index]]
    expanded_coord = list(expanded$coord[[1]],expanded$coord[[2]])
    expanded_path = expanded$path
    expanded_path = append(expanded_path,list(expanded_coord))
  }

  expanded_cost = expanded$g

  
  # Check if the A* reached to the target node
  if((currentNode[[1]] == goalNode[1]) && (currentNode[[2]] == goalNode[2])){
    
    # Initial/current coordinate
    current_coord = Frontier[[index]]$path[[1]]
    
    if(length(Frontier[[index]]$path)>1){
      # Retrieve the first movement node coordinate
      nextMove = Frontier[[index]]$path[[2]] 
    }
    else{
      nextMove = goalNode
    }
    
    # Check vertical movement
    if((current_coord[[2]] - nextMove[[2]]) < 0 ){
      return(8)
    } else if((current_coord[[2]] - nextMove[[2]]) > 0 ){
      return (2)
    }
      
    # Check horizontal movement
    if((current_coord[[1]] - nextMove[[1]]) < 0 ){
      return(6)
    } else if((current_coord[[1]] - nextMove[[1]]) > 0){
      return (4)
    }
    
    return(5)
  }
  
  # Pop the expanded node from the list 
  Frontier = Frontier[-index]

  
  # Above Node
  if(expanded_coord[2] < 10){
    
    new_coord = list(expanded_coord[[1]],expanded_coord[[2]]+1)
    traffic_cost =  trafficMatrix$vroads[expanded_coord[[1]],expanded_coord[[2]]] + expanded_cost
    heuristic_distance = abs(new_coord[[1]] - goalNode[1]) + abs((new_coord[[2]]) - goalNode[2])
    
    # Check if the neighbor node already exists in the frontier
    # test is a boolean list
    test = lapply(Frontier, function(item) item$coord[[1]] == new_coord[1] && item$coord[[2]] == new_coord[2])
    
    # Check if TRUE exists in the list "test", ie. one of the nodes (coordinates) are already in the Frontier
    if (is.element(TRUE,test)){
      true_index = grep(TRUE,test)
      existing_cost = Frontier[[true_index]]$g + Frontier[[true_index]]$h
      new_cost = traffic_cost + heuristic_distance
      
      if (new_cost < existing_cost){
        Frontier = Frontier[-true_index]
        newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
        Frontier = append(Frontier,list(newNode))
      }
      
    } else {
      newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
      Frontier = append(Frontier,list(newNode))
    }
  
   
  }

  # Below Node
  if(expanded_coord[2] > 1){
    
    new_coord = list(expanded_coord[[1]],expanded_coord[[2]]-1)
    traffic_cost = trafficMatrix$vroads[expanded_coord[[1]],expanded_coord[[2]]-1] + expanded_cost
    heuristic_distance = abs(new_coord[[1]] - goalNode[1]) + abs((new_coord[[2]]) - goalNode[2])
  
    # Check if the neighbor node already exists in the frontier
    # test is a boolean list
    test = lapply(Frontier, function(item) item$coord[[1]] == new_coord[1] && item$coord[[2]] == new_coord[2])
  
    # Check if TRUE exists in the list "test", ie. one of the nodes (coordinates) are already in the Frontier
    if (is.element(TRUE,test)){
      true_index = grep(TRUE,test)
      existing_cost = Frontier[[true_index]]$g + Frontier[[true_index]]$h
      new_cost = traffic_cost + heuristic_distance
      
      if (new_cost < existing_cost){
        Frontier = Frontier[-true_index]
        newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
        Frontier = append(Frontier,list(newNode))
      }
      
    } else {
      newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
      Frontier = append(Frontier,list(newNode))
    }
  }
  
  # Left Node
  if(expanded_coord[1] > 1){
    
    new_coord = list(expanded_coord[[1]]-1,expanded_coord[[2]])
    traffic_cost = trafficMatrix$hroads[expanded_coord[[1]]-1,expanded_coord[[2]]] + expanded_cost
    heuristic_distance = abs((new_coord[[1]]) - goalNode[1]) + abs(new_coord[[2]] - goalNode[2])
    
    
    # Check if the neighbor node already exists in the frontier
    # test is a boolean list
    test = lapply(Frontier, function(item) item$coord[[1]] == new_coord[1] && item$coord[[2]] == new_coord[2])
    
    # Check if TRUE exists in the list "test", ie. one of the nodes (coordinates) are already in the Frontier
    if (is.element(TRUE,test)){
      true_index = grep(TRUE,test)
      existing_cost = Frontier[[true_index]]$g + Frontier[[true_index]]$h
      new_cost = traffic_cost + heuristic_distance
      
      if (new_cost < existing_cost){
        Frontier = Frontier[-true_index]
        newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
        Frontier = append(Frontier,list(newNode))
      }
      
    } else {
      newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
      Frontier = append(Frontier,list(newNode))
    }
  }
  
  # Right Node
  if(expanded_coord[1] < 10){
    
    new_coord = list(expanded_coord[[1]] + 1,expanded_coord[[2]])
    traffic_cost = trafficMatrix$hroads[expanded_coord[[1]],expanded_coord[[2]]] + expanded_cost
    heuristic_distance = abs((new_coord[[1]]) - goalNode[1]) + abs(new_coord[[2]] - goalNode[2])
  
    
    # Check if the neighbor node already exists in the frontier
    # test is a boolean list
    test = lapply(Frontier, function(item) item$coord[[1]] == new_coord[1] && item$coord[[2]] == new_coord[2])
    
    # Check if TRUE exists in the list "test", ie. one of the nodes (coordinates) are already in the Frontier
    if (is.element(TRUE,test)){
      true_index = grep(TRUE,test)
      existing_cost = Frontier[[true_index]]$g + Frontier[[true_index]]$h
      new_cost = traffic_cost + heuristic_distance
      
      if (new_cost < existing_cost){
        Frontier = Frontier[-true_index]
        newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
        Frontier = append(Frontier,list(newNode))
      }
      
    } else {
      newNode = list(coord=new_coord,g=traffic_cost,h=heuristic_distance,path=expanded_path)
      Frontier = append(Frontier,list(newNode))
    }
  }
  
  
  # Recursively call the same function
  A_Search(Frontier,trafficMatrix,currentNode,goalNode)
  
}
