
count_groups <- function(rand_stop, race) {
  count <- NULL
  for (k in 1:length(race)) {
    count <- c(count, sum(rand_stop==race[k]))
  }
  return(count)
}



sim_rand_stops <- function(race,location,dist_pop_loc,nstops_loc,nsim=1000,seed=5){
  set.seed(seed)
  data <- NULL
  nloc <- length(location)
  nrace <- length(race)

  for (i in 1:nsim) {
    for (j in 1:nloc) {
      #simulate stop if randomly drawn from pppulation
      rand_stop <- sample(x=race,size = nstops_loc[j], replace = TRUE, prob = dist_pop_loc[,j])
      data <- c(data,count_groups(rand_stop, race) )
    }
  }

  #Create a 3-d array to store results
  sim_result <- array(data, dim = c(nrace,nloc,nsim),
                      dimnames=list(race,location,1:nsim))

  return(sim_result)
}



agg_locations <- function(sim_result,race, loc,nsim, nrace,prop=TRUE){
  data.loc_agg <- data.frame(matrix(ncol = nrace+1, nrow = nsim))
  colnames(data.loc_agg) <- c('simID', race)

  for (i in 1:nsim) {
    if (length(loc)==1) {
      temp_data <- sim_result[,loc,i]/(ifelse(prop==TRUE,sum(sim_result[,loc,i]),1))
      data.loc_agg[i,]<- c(i,temp_data)
    } else {
      temp_data <- rowSums(sim_result[,loc,i], dims = 1)/(ifelse(prop==TRUE,sum(rowSums(sim_result[,loc,i], dims = 1)),1))
      data.loc_agg[i,]<- c(i,temp_data)
    }
  }
  return(data.loc_agg)
}




randomize_test <- function(sim_data, group, non= FALSE, obs_stops){
  sim_data <- sim_data[,-1]
  require(dplyr)
  ####for group######
  if (non==FALSE) {
    data_test <- sim_data[,group]
  } else{

    ####for non-group#####
    #Select groups
    data_test <- sim_data %>%
      select(!all_of(group))
    #sum across groups
    data_test <- data_test%>%
      mutate(sum = rowSums(.[1:ncol(data_test)]))
    #retrieving sum
    data_test <- data_test[,ncol(data_test)]


  }

  ###testing###
  nsim <- length(data_test)
  sim_pvalue<- sum(data_test>obs_stops)/nsim

  #Output
  print("****************Output**************")
  print(ifelse(non==FALSE,paste0("Group: ",group),paste0("Group: ","Non-",group)))
  print("H_A: Rand_stop > observed_stop")
  print(paste0("simulated p-value=",sim_pvalue))
  hist(data_test, breaks=30, col='grey', main=paste0("Randomized Distribution of Traffic Stops for ",
                                                     ifelse(non==FALSE,paste0(group," Racial Group"),paste0("Non-",group, " Racial Group"))),
       las=1, xlab='', xlim = range(c(obs_stops,range(data_test))))
  abline(v=obs_stops, lwd=3, col="red")
  mtext(paste0("simulated p-value=",sim_pvalue), side=3)

}

#' Randomization Test for Racial Bias
#'
#' Used to test whether there is any bias in traffic stops of a police officer using randomization test
#' @param race vector containing all racial groups;
#' @param location vector containing all locations where there are traffic stops;
#' @param dis_pop_loc matrix containing proportion of each race to the DPE per location. race -- rows; location -- columns;
#' @param nstops_loc vector containing number of stops per location;
#' @param nsim number of simulation;
#' @param seed seed used for simulation for reproducibility;
#' @param loc location(s) to use for analysis;
#' @param prop Indicate whether the simulation should be shown as proportion or count;
#' @param group racial group to conduct test for;
#' @param non Used to indicate when interest in say non-white or non-hispanic grouping for test;
#' @param obs_stops number/proportion of stops observed in data for the group being tested

#' @return Simulated p-value and graph showing a randomized distribution of traffic stops
#' @examples
#' race <- c("Asian","Black","Hisp","White","Others");
#' location <- c("Sunderland","Amherst","Pelham");
#' dist_pop_loc <- matrix(c(0.075,0.093, 0.128,0.701,0.003,0.075,0.093, 0.128,0.701,0.003,0.075,0.093, 0.128,0.701,0.003), nrow=5, byrow=FALSE);
#' nstops_loc <- c(50,30,20);
#' nsim <-1000;
#' loc <- c("Amherst","Sunderland");
#' group="Black";
#' obs_stops <- 8;
#' randomize_test_bias(race,location,dist_pop_loc,nstops_loc,nsim=1000,seed=5,loc,prop=FALSE,group,non=FALSE,obs_stops);

#' @export
randomize_test_bias <- function(race,location,dist_pop_loc,nstops_loc,nsim=1000,seed=5,loc,prop=TRUE,group,non=FALSE,obs_stops){

  sim_result <- sim_rand_stops (race,location,dist_pop_loc,nstops_loc,nsim,seed)
  sim_data <- agg_locations(sim_result,race, loc,nsim, nrace=length(race),prop)
  randomize_test(sim_data, group, non, obs_stops)

}

