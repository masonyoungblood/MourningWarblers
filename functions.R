#create learning function
learn <- function(ind, pop, grids, mig_mat, potential_migrants, geo, dems, rep_size, total_possible_syls, mu, frequency, attr, content, cutoff){
  #if potential migrants is not an empty object, then bird will have one migrant demonstrator
  if(!is.null(potential_migrants)){
    #sample local demonstrators based on their geographic locations
    local_dems <- sample(order(grids[[ind]]$dists[geo, pop[[ind]]$geo])[1:round((nrow(pop[[ind]])*cutoff))], dems[ind]-1)
    
    #get possible syllables, which includes all syllables from local demonstrators plus syllables from one migrant demonstrator
    possible_syls <- c(pop[[ind]]$syls[local_dems], potential_migrants[sample(1:length(potential_migrants), 1)])
  } else{
    #otherwise, it is only local demonstrators
    local_dems <- sample(order(grids[[ind]]$dists[geo, pop[[ind]]$geo])[1:round((nrow(pop[[ind]])*cutoff))], dems[ind])
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
model <- function(n_init = c(1000, 1000, 1000, 1000), pop_rate = 0.989, mortality = 0.5, geo_res = list(c(50, 20), c(50, 20), c(10, 10), c(10, 10)), regio_dists, total_possible_syls,
                  years = c(1985, 2006, 2018), inds_to_sample = list(c(60, 164, 181), c(76, 219, 227), c(47, 92, 100), c(13, 24, 32)), 
                  dems = c(3, 3, 3, 3), rep_size = 3, mu = 0.01, frequency = c(1, 1, 1, 1), content = c(0, 0, 0, 0), cutoff = 0.1){
  #set up immigration object, where rows are sources and columns are destinations, in the same order as geo_res
  #imm_prob is the average probability of immigration
  #source_edge is the edge of the source regiolect that birds are coming from (location of potential immigrant demonstrators)
  #dest_edge is the edge of the destination regiolect that birds are going to (location of focal learners)
  imm_prob <- matrix(c(0, 0.0675, 0, 0, 0.0471, 0, 0.0034, 0, 0, 0, 0, 0.0065, 0, 0, 0.0101, 0), 4, 4, dimnames = list(c("west", "east", "newf", "nova"), c("west", "east", "newf", "nova")))
  source_edge <- matrix(c(NA, "L", NA, NA, "R", NA, "L", NA, NA, NA, NA, "T", NA, NA, "B", NA), 4, 4, dimnames = list(c("west", "east", "newf", "nova"), c("west", "east", "newf", "nova")))
  dest_edge <- matrix(c(NA, "R", NA, NA, "L", NA, "R", NA, NA, NA, NA, "B", NA, NA, "T", NA), 4, 4, dimnames = list(c("west", "east", "newf", "nova"), c("west", "east", "newf", "nova")))
  immigration <- list(imm_prob = imm_prob, source_edge = source_edge, dest_edge = dest_edge)
  
  #generate grids for each regiolect
  grids <- lapply(1:4, function(x){
    temp <- expand.grid(1:geo_res[[x]][1], 1:geo_res[[x]][2])
    colnames(temp) <- c("x", "y")
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
      
      #get number of immigrants from each regiolect
      num_imm_per_regio <- round(immigration$imm_prob[, x]*(pop_sizes[[x]][i]-nrow(pop[[x]])))
      
      #store all imigration events from all regiolects within immigration
      immigration_events <- do.call(rbind, lapply(as.numeric(which(num_imm_per_regio > 0)), function(y){
        #store learners who live closest to the edge who will have one immigrant demonstrator
        if(immigration$dest_edge[y, x] == "R"){edge_birds <- order(grids[[x]]$coords$x[geos], decreasing = TRUE)[1:num_imm_per_regio[y]]}
        if(immigration$dest_edge[y, x] == "L"){edge_birds <- order(grids[[x]]$coords$x[geos], decreasing = FALSE)[1:num_imm_per_regio[y]]}
        if(immigration$dest_edge[y, x] == "T"){edge_birds <- order(grids[[x]]$coords$y[geos], decreasing = TRUE)[1:num_imm_per_regio[y]]}
        if(immigration$dest_edge[y, x] == "B"){edge_birds <- order(grids[[x]]$coords$y[geos], decreasing = FALSE)[1:num_imm_per_regio[y]]}
        
        #store pool of potential migrant demonstrators who are on edge of neighboring territory
        if(immigration$source_edge[y, x] == "R"){potential_migrants <- pop[[y]]$syls[order(grids[[y]]$coords$x[pop[[y]]$geo], decreasing = TRUE)[1:(nrow(pop[[y]])*cutoff)]]}
        if(immigration$source_edge[y, x] == "L"){potential_migrants <- pop[[y]]$syls[order(grids[[y]]$coords$x[pop[[y]]$geo], decreasing = FALSE)[1:(nrow(pop[[y]])*cutoff)]]}
        if(immigration$source_edge[y, x] == "T"){potential_migrants <- pop[[y]]$syls[order(grids[[y]]$coords$y[pop[[y]]$geo], decreasing = TRUE)[1:(nrow(pop[[y]])*cutoff)]]}
        if(immigration$source_edge[y, x] == "B"){potential_migrants <- pop[[y]]$syls[order(grids[[y]]$coords$y[pop[[y]]$geo], decreasing = FALSE)[1:(nrow(pop[[y]])*cutoff)]]}
        
        #return a data table where each row is an edge bird, and the potential migrants are stored separately for each edge bird
        #this seems excessive, but the reasoning is that it allows us to combine all immigration events from all source regiolects in a single object
        return(data.table::data.table(edge_birds = edge_birds, potential_migrants = lapply(1:length(edge_birds), function(z){potential_migrants})))
      }))
      immigration_events <- immigration_events[-which(duplicated(immigration_events$edge_birds)), ]
      
      #construct data table with new birds
      data.table::data.table(syls = lapply(1:(pop_sizes[[x]][i]-nrow(pop[[x]])), function(y){
        #if this bird will learn from a migrant demonstrator, then store all possible migrant demonstrators
        if(y %in% immigration_events$edge_birds){
          potential_migrants <- immigration_events$potential_migrants[match(y, immigration_events$edge_birds)]
        } else{
          potential_migrants <- NULL
        }
        
        #learning occurs
        learn(ind = x, pop, grids, mig_mat, potential_migrants, geos[y], dems, rep_size, total_possible_syls, mu, frequency, attr, content, cutoff)
      }), geo = geos)
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
