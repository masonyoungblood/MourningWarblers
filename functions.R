#load libraries
library(Rcpp)

#create learning function
learn <- function(pop, geo, dems, rep_size, total_possible_syls, mu, frequency, attr, content){
  #calculate probabilities of learning from each demonstrator
  dem_probs <- 1/(abs(geo-pop$geo)+1)

  #choose demonstrators
  chosen_dems <- sample(nrow(pop), dems, prob = dem_probs)

  #get table of syllables from demonstrators
  syl_table <- Rfast::Table(unlist(pop$syls[chosen_dems]))

  #create new repertoire (sample only if there are multiple variants available)
  if(length(as.numeric(names(syl_table))) > 1){
    new_rep <- unique(sample(as.numeric(names(syl_table)), rep_size, prob = (as.numeric(syl_table)^frequency)*(attr[as.numeric(names(syl_table))]^content), replace = TRUE))
  } else{
    new_rep <- as.numeric(names(syl_table))
  }
  
  #simulate innovation
  innov_bool <- rbinom(length(new_rep), 1, prob = c(mu, 1-mu))
  new_rep[which(innov_bool == 1)] <- sample(total_possible_syls, sum(innov_bool))

  #return repertoire
  return(new_rep)
}

#varying parameters to be estimated by the model include n_init (unif: 1,000-10,000), mu (prior TBD), frequency (trunc_norm: 1, 1)
#specify model
model <- function(n_init, pop_rate = 0.998, mortality = 0.5, geo_res = 100, syl_init, total_possible_syls, years = c(1985, 2006, 2018), inds_to_sample, dems = 2, rep_size = 3, mu, frequency, content){
  #generate vector of population sizes
  pop_sizes <- c(n_init)
  for(i in 1:(length(range(years)[1]:range(years)[2])-1)){pop_sizes[i+1] <- round(pop_sizes[i]*pop_rate)}
  
  #generate vector of mortalities
  mortalities <- round(pop_sizes*mortality)
  
  #generate syllable attractiveness indices
  attr <- truncnorm::rtruncnorm(total_possible_syls, a = 0, mean = 1, sd = 0.5)
  
  #generate starting population
  pop <- data.table::data.table(syls = sapply(1:pop_sizes[1], function(x){unique(sample(length(syl_init), rep_size, replace = TRUE, prob = syl_init))}), geo = sample(geo_res, pop_sizes[1], replace = TRUE))
  
  #create list to store sampled frequency distribution
  output <- list()
  
  #simulate first year of mortality
  pop <- pop[-sample(nrow(pop), mortalities[1]), ]
  
  #iterate loop
  for(i in 2:length(range(years)[1]:range(years)[2])){
    #create new generation of birds
    geos <- sample(geo_res, pop_sizes[i]-nrow(pop), replace = TRUE)
    new_gen <- data.table::data.table(syls = lapply(1:(pop_sizes[i]-nrow(pop)), function(x){learn(pop, geos[x], dems, rep_size, total_possible_syls, mu, frequency, attr, content)}), geo = geos)
    
    #add new generation to existing population
    pop <- data.table::rbindlist(list(pop, new_gen))
    
    #if current year is in observed data then collect output
    if((range(years)[1]:range(years)[2])[i] %in% years){
      #collect the frequency distribution of syllables from the same the number of individuals observed in the real data in the current year
      output[[length(output)+1]] <- sort(as.numeric(table(unlist(pop$syls[sample(nrow(pop), inds_to_sample[which(years == (range(years)[1]:range(years)[2])[i])])]))), decreasing = TRUE)
    }
    
    #simulate mortality
    pop <- pop[-sample(nrow(pop), mortalities[i]), ]
  }
  
  #return output
  return(output)
}
