#create learning function
learn <- function(ind, pop, grids, mig_mat, edge_birds, potential_migrants, geo, dems, rep_size, total_possible_syls, mu, frequency, attr, content, cutoff){
  #compute how many migrant demonstrators a bird will learn from
  if(edge_birds){
    if(ind == 1){n_migrant_dems <- sum(sample(c(1, 0), dems[ind], replace = TRUE, prob = c(mig_mat[2, 1], 1-mig_mat[2, 1])))}
    if(ind == 2){n_migrant_dems <- sum(sample(c(1, 0), dems[ind], replace = TRUE, prob = c(mig_mat[1, 2], 1-mig_mat[1, 2])))}
    if(ind == 3){n_migrant_dems <- sum(sample(c(1, 0), dems[ind], replace = TRUE, prob = c(mig_mat[4, 3], 1-mig_mat[4, 3])))}
    if(ind == 4){n_migrant_dems <- sum(sample(c(1, 0), dems[ind], replace = TRUE, prob = c(mig_mat[3, 4], 1-mig_mat[3, 4])))}
  } else{
    n_migrant_dems <- 0
  }
  
  #if all of their demonstrators are not migrants, then store the number of local demonstrators
  if(n_migrant_dems < dems[ind]){
    local_dems <- sample(order(grids[[ind]]$dists[geo, pop[[ind]]$geo])[1:round((nrow(pop[[ind]])*cutoff))], dems[ind]-n_migrant_dems)
  }
  
  #if at least one demonstrator is a migrant but all are not, then combine possible syllables from local and migrant demonstrators
  if(n_migrant_dems > 0 & n_migrant_dems < dems[ind]){
    if(ind == 1){possible_syls <- c(pop[[ind]]$syls[local_dems], pop[[2]]$syls[sample(potential_migrants, n_migrant_dems)])}
    if(ind == 2){possible_syls <- c(pop[[ind]]$syls[local_dems], pop[[1]]$syls[sample(potential_migrants, n_migrant_dems)])}
    if(ind == 3){possible_syls <- c(pop[[ind]]$syls[local_dems], pop[[4]]$syls[sample(potential_migrants, n_migrant_dems)])}
    if(ind == 4){possible_syls <- c(pop[[ind]]$syls[local_dems], pop[[3]]$syls[sample(potential_migrants, n_migrant_dems)])}
  }
  
  #if all demonstrators are migrants, then only store those
  if(n_migrant_dems == dems[ind]){
    if(ind == 1){possible_syls <- pop[[2]]$syls[sample(potential_migrants, n_migrant_dems)]}
    if(ind == 2){possible_syls <- pop[[1]]$syls[sample(potential_migrants, n_migrant_dems)]}
    if(ind == 3){possible_syls <- pop[[4]]$syls[sample(potential_migrants, n_migrant_dems)]}
    if(ind == 4){possible_syls <- pop[[3]]$syls[sample(potential_migrants, n_migrant_dems)]}
  } 
  
  #if none of the demonstrators are migrants, then only store syllables from local demonstrators
  if(n_migrant_dems == 0){
    possible_syls <- pop[[ind]]$syls[local_dems]
  }
  
  #get table of syllables from demonstrators
  syl_table <- tabulate(as.numeric(unlist(possible_syls)), nbins = total_possible_syls)
  
  #create new repertoire (sample only if there are multiple variants available)
  if(length(which(syl_table > 0)) > 1){
    new_rep <- unique(sample(total_possible_syls, rep_size, prob = (syl_table^frequency[ind])*(attr^content[ind]), replace = TRUE))
  } else{
    new_rep <- which(syl_table > 0)
  }
  
  #simulate innovation
  innov_bool <- sample(c(1, 0), length(new_rep), replace = TRUE, prob = c(mu, 1-mu))
  new_rep[which(innov_bool == 1)] <- sample(total_possible_syls, sum(innov_bool))
  
  #return repertoire
  return(new_rep)
}

#POPULATION SIZES IN EACH YEAR AND LOCATION
#west = 60, 164, 181
#east = 76, 219, 227
#newf = 47, 92, 100
#nova = 13, 24, 32

#varying parameters to be estimated by the model include n_init, mu, dems, frequency, and content
#pop_rate = 0.989 is from an annual trend of -1.1% between 1966-2015 from Sauer et al. (2017): https://doi.org/10.1650/CONDOR-17-83.1
#geo_res is the geographic resolutions of the western, eastern, newfoundland, and nova scotia regiolects, in that order
#mig_mat is a matrix of migration probabilities, where rows are the source and columns are the destination, in the same order as geo_res
#the model assumes that migration only occurs between west and east, and between newfoundland and nova scotia
#specify model
model <- function(n_init = c(1000, 1000, 1000, 1000), pop_rate = 0.989, mortality = 0.5, geo_res = list(c(50, 20), c(50, 20), c(10, 10), c(10, 10)), regio_dists,
                  mig_mat = matrix(c(0, 0.077968, 0, 0, 0.051011, 0, 0, 0, 0, 0, 0, 0.006601, 0, 0, 0.008547, 0), 4, 4, dimnames = list(c("west", "east", "newf", "nova"), c("west", "east", "newf", "nova"))), 
                  years = c(1985, 2006, 2018), inds_to_sample = list(c(60, 164, 181), c(76, 219, 227), c(47, 92, 100), c(13, 24, 32)), dems = c(3, 3, 3, 3), rep_size = 3, mu = 0.01, frequency = c(1, 1, 1, 1), content = c(0, 0, 0, 0), cutoff = 0.1){
  #calculate total number of possible syllables
  total_possible_syls <- length(regio_dists[[1]])
  
  #generate grids for each regiolect
  grids <- lapply(1:4, function(x){
    temp <- expand.grid(1:geo_res[[x]][1], 1:geo_res[[x]][2])
    colnames(temp) <- c("x", "y")
    if(x == 1){temp$edge <- ifelse(temp$x == geo_res[[x]][1], 1, 0)}
    if(x == 2){temp$edge <- ifelse(temp$x == 1, 1, 0)}
    if(x == 3){temp$edge <- ifelse(temp$y == 1, 1, 0)}
    if(x == 4){temp$edge <- ifelse(temp$y == geo_res[[x]][2], 1, 0)}
    dists <- as.matrix(dist(temp[, c(1:2)]))
    return(list(coords = temp, dists = dists))
  })
  
  #generate vector of population sizes
  pop_sizes <- lapply(1:4, function(x){
    temp <- n_init[x]
    for(i in 1:(length(range(years)[1]:range(years)[2])-1)){temp[i+1] <- round(temp[i]*pop_rate)}
    return(temp)
  })
  
  #generate vector of mortalities
  mortalities <- lapply(1:4, function(x){round(pop_sizes[[x]]*mortality)})
  
  #generate syllable attractiveness indices
  attr <- truncnorm::rtruncnorm(total_possible_syls, a = 0, mean = 1, sd = 0.5)
  
  #generate starting populations
  pop <- lapply(1:4, function(x){
    data.table::data.table(syls = sapply(1:pop_sizes[[x]][1], function(y){unique(sample(total_possible_syls, rep_size, replace = TRUE, prob = regio_dists[[x]]))}), geo = sample(prod(geo_res[[x]]), pop_sizes[[x]][1], replace = TRUE))
  })
  
  #create list to store sampled frequency distribution
  output <- list()
  
  #simulate first year of mortality
  for(i in 1:4){pop[[i]] <- pop[[i]][-sample(nrow(pop[[i]]), mortalities[[i]][1]), ]}
  
  #iterate loop
  for(i in 2:length(range(years)[1]:range(years)[2])){
    #create new generation of birds
    new_gen <- lapply(1:4, function(x){
      #sample new geographic indices
      geos <- sample(prod(geo_res[[x]]), pop_sizes[[x]][i]-nrow(pop[[x]]), replace = TRUE)
      
      #identify which new birds are on the edge of the territory
      if(x %in% c(1, 2)){edge_birds <- order(abs(grids[[x]]$coords$x[geos] - unique(grids[[x]]$coords$x[which(grids[[x]]$coords$edge == 1)])))[1:round((nrow(pop[[x]])*cutoff))]}
      if(x %in% c(3, 4)){edge_birds <- order(abs(grids[[x]]$coords$y[geos] - unique(grids[[x]]$coords$y[which(grids[[x]]$coords$edge == 1)])))[1:round((nrow(pop[[x]])*cutoff))]}
      
      #identify which old birds are on the edge of the neighboring territory
      if(x == 1){potential_migrants <- order(abs(grids[[2]]$coords$x[geos] - unique(grids[[2]]$coords$x[which(grids[[2]]$coords$edge == 1)])))[1:round((nrow(pop[[2]])*cutoff))]}
      if(x == 2){potential_migrants <- order(abs(grids[[1]]$coords$x[geos] - unique(grids[[1]]$coords$x[which(grids[[1]]$coords$edge == 1)])))[1:round((nrow(pop[[1]])*cutoff))]}
      if(x == 3){potential_migrants <- order(abs(grids[[4]]$coords$y[geos] - unique(grids[[4]]$coords$y[which(grids[[4]]$coords$edge == 1)])))[1:round((nrow(pop[[4]])*cutoff))]}
      if(x == 4){potential_migrants <- order(abs(grids[[3]]$coords$y[geos] - unique(grids[[3]]$coords$y[which(grids[[3]]$coords$edge == 1)])))[1:round((nrow(pop[[3]])*cutoff))]}
      
      #construct data table with new birds
      data.table::data.table(syls = lapply(1:(pop_sizes[[x]][i]-nrow(pop[[x]])), function(y){learn(ind = x, pop, grids, mig_mat, edge_birds = y %in% edge_birds, potential_migrants, geos[y], dems, rep_size, total_possible_syls, mu, frequency, attr, content, cutoff)}), geo = geos)
    })

    #add new generation to existing population
    for(j in 1:4){pop[[j]] <- data.table::rbindlist(list(pop[[j]], new_gen[[j]]))}
    
    #if current year is in observed data then collect output
    if((range(years)[1]:range(years)[2])[i] %in% years){
      #collect the frequency distribution of syllables from the same the number of individuals observed in the real data in the current year
      output[[length(output)+1]] <- lapply(1:4, function(x){
        tabulate(unlist(pop[[x]]$syls[sample(nrow(pop[[x]]), inds_to_sample[[x]][which(years == (range(years)[1]:range(years)[2])[i])])]), nbins = total_possible_syls)
      })
    }
    
    #simulate mortality
    for(j in 1:4){pop[[j]] <- pop[[j]][-sample(nrow(pop[[j]]), mortalities[[j]][i]), ]}
  }
  
  #return output
  return(output)
}
