################################################################################
################################################################################
#           SCRIPT 4 - COMPARING THE DD WITH KNOWN DISTRIBUTIONS
################################################################################
################################################################################

#FMestre
#09-10-2022

#Acording to our last meeting Miguel is of the opinion that we should compare 
#both DD wit known distributions

#Degree distributions
overall_degree_distribution_list
in_degree_distribution_list
out_degree_distribution_list

#Use these networks
final_data_frame_16_FW$network_number
final_data_frame_16_MUT$network_number


################################################################################
#                          PLOT DEGREE DISTRIBUTIONS
################################################################################

#Load packages
library(igraph)

#PLOT
pdf(file="degree_distributions_1_CUMULATIVE_vers2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 1:50){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_2_CUMULATIVE_vers2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 51:100){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_3_CUMULATIVE_vers2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 101:150){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_4_CUMULATIVE_vers2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 151:200){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_5_CUMULATIVE_vers2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 201:250){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_6_CUMULATIVE_vers2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 251:300){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_7_CUMULATIVE_vers2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,2), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 301:310){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  lines(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

