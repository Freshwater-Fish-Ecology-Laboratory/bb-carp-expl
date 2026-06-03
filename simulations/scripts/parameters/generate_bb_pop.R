# Burbot tag-recovery simulations - Module 1 - code to simulate a burbot population. 
# edited Jan 18 2024

# parameters: 
## lake surface area
## burbot density in fish/ha 
## burbot density in kg/ha
## estimate of mean burbot weight

SA_km2 <- 56.3
density_wt <- 0.26
density_n <- 0.2363636
mean_wt_bb <- 1.1

# returning number of burbot via both weight and abundance data
SA_n_bb <- function(SA_km2, density_n, density_wt, mean_wt_bb){
  SA_ha <- (SA_km2*100)
  n_bb1 <- (density_n*SA_ha)
  kg_bb <- (SA_ha*density_wt)
  n_bb2 <- (kg_bb/mean_wt_bb)
  return(c(n_bb1, n_bb2))
}

SA_n_bb(SA_km2, density_n, density_wt, mean_wt_bb)

# NOTE: try filtering the lake surface area by depth strata and use different
# burbot density values weighted by the surface area for that depth
