################################################################################
################################################################################
#           SCRIPT 4 - COMPARING THE DD WITH KNOWN DISTRIBUTIONS
################################################################################
################################################################################

#FMestre
#09-10-2022

#Load packages
library(igraph)
library(rmangal)

################################################################################
# 1.Select degree distributions used
################################################################################

#Degree distributions
overall_degree_distribution_list
in_degree_distribution_list
out_degree_distribution_list

#Use these networks
final_data_frame_16_FW$network_number
final_data_frame_16_MUT$network_number

fw_ids_used <- c()
for(i in 1:length(final_data_frame_16_FW$network_number)) fw_ids_used[i] <- as.numeric(stringr::str_split(final_data_frame_16_FW$network_number[i], pattern = "#")[[1]][2])

mut_ids_used <- c()
for(i in 1:length(final_data_frame_16_MUT$network_number)) mut_ids_used[i] <- as.numeric(stringr::str_split(final_data_frame_16_MUT$network_number[i], pattern = "#")[[1]][2])

ids_used <- c(fw_ids_used, mut_ids_used)


overall_degree_distribution_list_RETAINED <- list()
in_degree_distribution_list_RETAINED <- list()
out_degree_distribution_list_RETAINED <- list()
#
names_networks <- c()

for(i in 1:length(ids_used)){
  
id_used_1 <- ids_used[i]

id_ant <- paste0("antagonistic_Network #", id_used_1)
id_mut <- paste0("mutualistic_Network #", id_used_1)

if (id_ant %in% names(overall_degree_distribution_list)) id_overall <- which(names(overall_degree_distribution_list) == id_ant)
if (id_mut %in% names(overall_degree_distribution_list)) id_overall <- which(names(overall_degree_distribution_list) == id_mut)

if (id_ant %in% names(in_degree_distribution_list)) id_in <- which(names(in_degree_distribution_list) == id_ant)
if (id_mut %in% names(in_degree_distribution_list)) id_in <- which(names(in_degree_distribution_list) == id_mut)

if (id_ant %in% names(out_degree_distribution_list)) id_out <- which(names(out_degree_distribution_list) == id_ant)
if (id_mut %in% names(out_degree_distribution_list)) id_out <- which(names(out_degree_distribution_list) == id_mut)

if (id_overall == id_in && id_overall == id_out){
  
  overall_degree_distribution_list_RETAINED[[i]] <- overall_degree_distribution_list[[id_overall]]
  in_degree_distribution_list_RETAINED[[i]] <- in_degree_distribution_list[[id_overall]]
  out_degree_distribution_list_RETAINED[[i]] <- out_degree_distribution_list[[id_overall]]
  
  names_networks[i] <- paste0("network_", id_used_1)
  
} else stop("Different ids!")

message(i)

}


names(overall_degree_distribution_list_RETAINED) <- names_networks
names(in_degree_distribution_list_RETAINED) <- names_networks
names(out_degree_distribution_list_RETAINED) <- names_networks


################################################################################
# 2.Select cheddar objects
################################################################################

mangal_objects_selected_10NOV23 <- list()
names_cheddar <- c()

for(i in 1:length(all_mangal_objects_selected)){
  
  if(all_mangal_objects_selected[[i]]$network$network_id %in% ids_used){
    
    names_cheddar <- append(names_cheddar, all_mangal_objects_selected[[i]]$network$network_id)
    mangal_objects_selected_10NOV23 <- append(mangal_objects_selected_10NOV23, all_mangal_objects_selected[i])
    
  }
  
 message(i)
  
}

length(names_cheddar)
length(mangal_objects_selected_10NOV23)

names_cheddar <- paste0("network_", names_cheddar)
names(mangal_objects_selected_10NOV23) <- names_cheddar

#Convert to Igraph

mangal_objects_selected_10NOV23_IGRAPH <- list()

for(i in 1:length(mangal_objects_selected_10NOV23)){
mangal_objects_selected_10NOV23_IGRAPH[[i]] <- as.igraph(mangal_objects_selected_10NOV23[[i]])
}

names(mangal_objects_selected_10NOV23_IGRAPH) <- names_cheddar

################################################################################
# 3.Plot degree distributions
################################################################################

#PLOT
pdf(file="degree_distributions_1_CUMULATIVE_vers2.pdf",  width=8, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 1:50){
  
  plot(overall_degree_distribution_list_RETAINED[[i]], type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = names_networks[i])
  lines(in_degree_distribution_list_RETAINED[[i]], type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(out_degree_distribution_list_RETAINED[[i]], type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  #legend("topright", legend=c("Overall", "In-D", "Out-D"), col=c("black", "red", "blue"))
  #
  plot(mangal_objects_selected_10NOV23_IGRAPH[[i]], layout=layout_in_circle)
  
  }

dev.off()

###################### # ######################

#PLOT
pdf(file="degree_distributions_2_CUMULATIVE_vers2.pdf",  width=8, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 51:100){
  
  plot(overall_degree_distribution_list_RETAINED[[i]], type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = names_networks[i])
  lines(in_degree_distribution_list_RETAINED[[i]], type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(out_degree_distribution_list_RETAINED[[i]], type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  #
  plot(mangal_objects_selected_10NOV23_IGRAPH[[i]], layout=layout_in_circle)
  
}

dev.off()

###################### # ######################

#PLOT
pdf(file="degree_distributions_3_CUMULATIVE_vers2.pdf",  width=8, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 101:250){
  
  plot(overall_degree_distribution_list_RETAINED[[i]], type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = names_networks[i])
  lines(in_degree_distribution_list_RETAINED[[i]], type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(out_degree_distribution_list_RETAINED[[i]], type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  #
  plot(mangal_objects_selected_10NOV23_IGRAPH[[i]], layout=layout_in_circle)
  
}

dev.off()

###################### # ######################

#PLOT
pdf(file="degree_distributions_4_CUMULATIVE_vers2.pdf",  width=8, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 251:296){
  
  plot(overall_degree_distribution_list_RETAINED[[i]], type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = names_networks[i])
  lines(in_degree_distribution_list_RETAINED[[i]], type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(out_degree_distribution_list_RETAINED[[i]], type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  #
  plot(mangal_objects_selected_10NOV23_IGRAPH[[i]], layout=layout_in_circle)
  
}

dev.off()
