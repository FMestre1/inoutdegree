#FMestre
#16-03-2022

#Using rmangal

library(rmangal)
library(igraph)

#"mutualism", "predation", "herbivory", "scavenger", "detritivore"

mangal_mutualism <- search_interactions(type = "mutualism", verbose = TRUE)
mangal_predation <- search_interactions(type = "predation", verbose = TRUE)
mangal_herbivory <- search_interactions(type = "herbivory", verbose = TRUE)
mangal_scavenger <- search_interactions(type = "scavenger", verbose = TRUE)
mangal_detritivore <- search_interactions(type = "detritivore", verbose = TRUE)

#Select directed
mangal_mutualism_dir <-  mangal_mutualism[mangal_mutualism$direction == "directed",]
mangal_predation_dir <- mangal_predation[mangal_predation$direction == "directed",]
mangal_herbivory_dir <- mangal_herbivory[mangal_herbivory$direction == "directed",]
mangal_scavenger_dir <- mangal_scavenger[mangal_scavenger$direction == "directed",]
mangal_detritivore_dir <- mangal_detritivore[mangal_detritivore$direction == "directed",]

mangal_collection_mutualism_dir <- get_collection(mangal_mutualism_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_predation_dir <- get_collection(mangal_predation_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_herbivory_dir <- get_collection(mangal_herbivory_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_scavenger_dir <- get_collection(mangal_scavenger_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_detritivore_dir <- get_collection(mangal_detritivore_dir, as_sf = TRUE, verbose = TRUE)

mangal_collection <- combine_mgNetworks(mangal_collection_mutualism_dir, mangal_collection_predation_dir, 
                                        mangal_collection_herbivory_dir, mangal_collection_scavenger_dir, 
                                        mangal_collection_detritivore_dir)

#Save
save(mangal_collection, file="mangal_dataset_mangal.RData")

#Convert to igraph
mangal_collection_igraph <- as.igraph(mangal_collection)
save(mangal_collection_igraph, file="mangal_dataset_igraph.RData")

#Getting the "in" and "out" degree of all networks


in_degree_list <- list()
out_degree_list <- list()

for(i in 1:length(mangal_collection_igraph)){

in_degree_list[[i]] <- degree(mangal_collection_igraph[[i]], mode="in")
out_degree_list[[i]] <- degree(mangal_collection_igraph[[i]], mode="out")
  
message(paste0("Concluded matrix ", i, "!"))

}

mangal_collection_igraph_2 <- mangal_collection_igraph[as.numeric(lapply(in_degree_list, sum)) > as.numeric(lapply(in_degree_list, length))]

length(mangal_collection_igraph_2)

##

in_degree_list_2 <- list()
out_degree_list_2 <- list()

for(i in 1:length(mangal_collection_igraph_2)){
  
  in_degree_list_2[[i]] <- degree(mangal_collection_igraph_2[[i]], mode="in")
  out_degree_list_2[[i]] <- degree(mangal_collection_igraph_2[[i]], mode="out")
  
  message(paste0("Concluded matrix ", i, "!"))
  
}

#mangal_collection_igraph_2 <- mangal_collection_igraph[-c(53:63)]


alpha_in <- c()
alpha_out <- c()
p_in <- c()
p_out <- c()
log_lik_in <- c()
log_lik_out <- c()

for(i in 1:length(mangal_collection_igraph_2)){
  
fit_in <- fit_power_law(in_degree_list_2[[i]]+1)
fit_out <- fit_power_law(out_degree_list_2[[i]]+1)

 
alpha_in[i] <- fit_in$alpha
alpha_out[i] <- fit_out$alpha
p_in[i] <- fit_in$KS.p
p_out[i] <- fit_out$KS.p
log_lik_in[[i]] <- fit_in$logLik
log_lik_out[[i]] <- fit_out$logLik

message(paste0("Network ", i))

}



  



