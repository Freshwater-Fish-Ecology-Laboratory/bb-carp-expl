# Code to estimate cost of tagging n burbot for each sampling method
# From literature/research - determine the cost per hour (method_cost) of each main burbot capture method

get_cost <- function(cpue, method_cost){
  effort <- ((1/cpue) * tagged)
  cost <- (method_cost*effort)
  return(cost)
}

