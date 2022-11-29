#FMestre
#29-11-2022

library(devtools)
install_github("andrewheiss/reconPlots", force = TRUE)
library(reconPlots)



#Evaluate if there is intersection between in- and out-degree and where it is in terms 
#of node degree

#These are the networks
final_data_frame_16_MUT$network_number
final_data_frame_16_FW$network_number
#length(final_data_frame_16_MUT$network_number) + length(final_data_frame_16_FW$network_number)

#Networks, as igraph and as mangal objects
all_mangal_objects_selected_igraph
all_mangal_objects_selected

#Lists of vectors with the cumulative degree distributions
#names(overall_degree_distribution_list)
head(names(in_degree_distribution_list))
head(names(out_degree_distribution_list))

list_mu_in <- list()
list_ant_in <- list()
#
list_mu_out <- list()
list_ant_out <- list()

#MUT
for(i in 1:nrow(final_data_frame_16_MUT)){
  
  net_id <- final_data_frame_16_MUT$network_number[i]
  #
  list_mu_in[i] <- in_degree_distribution_list[stringr::str_detect(names(in_degree_distribution_list), paste0(net_id, "$"))]
  list_mu_out[i] <- out_degree_distribution_list[stringr::str_detect(names(out_degree_distribution_list), paste0(net_id, "$"))]
  
}

#ANT
for(i in 1:nrow(final_data_frame_16_FW)){
  
  net_id <- final_data_frame_16_FW$network_number[i]
  #
  list_ant_in[i] <- in_degree_distribution_list[stringr::str_detect(names(in_degree_distribution_list), paste0(net_id, "$"))]
  list_ant_out[i] <- out_degree_distribution_list[stringr::str_detect(names(out_degree_distribution_list), paste0(net_id, "$"))]
  
}

######################### Do they intersect? Where? ######################### 

intersection_points_mu <- list()
for(i in 1:nrow(final_data_frame_16_MUT)){

curve1 <- data.frame(0:(length(list_mu_in[[i]])-1), list_mu_in[[i]])
names(curve1) <- c("nr_nodes", "frequency")
#
curve2 <- data.frame(0:(length(list_mu_out[[i]])-1), list_mu_out[[i]])
names(curve2) <- c("nr_nodes", "frequency")

#plot(curve1, type="l")
#plot(curve2, type="l")

try(intersection_points_mu_loop <- curve_intersect(curve1, curve2))

if(exists("intersection_points")) intersection_points_mu[i] <- intersection_points_mu_loop else intersection_points_mu[i] <- NA 

}


intersection_points_ant <- list()
for(i in 1:nrow(final_data_frame_16_FW)){
  
  curve1 <- data.frame(0:(length(list_ant_in[[i]])-1), list_ant_in[[i]])
  names(curve1) <- c("nr_nodes", "frequency")
  #
  curve2 <- data.frame(0:(length(list_ant_out[[i]])-1), list_ant_out[[i]])
  names(curve2) <- c("nr_nodes", "frequency")
  
  #plot(curve1, type="l")
  #plot(curve2, type="l")
  
  try(intersection_points_ant_loop <- curve_intersect(curve1, curve2))
  
  if(exists("intersection_points")) intersection_points_ant[i] <- intersection_points_ant_loop else intersection_points_ant[i] <- NA 
  
}

#No intersection!


######################### exponents ######################### 

table(final_data_frame_16_MUT$alpha_in > final_data_frame_16_MUT$alpha_out)
table(final_data_frame_16_FW$alpha_in > final_data_frame_16_FW$alpha_out)

