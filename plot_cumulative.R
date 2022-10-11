#Plot Cumulative Degree Distribution
#FMestre
#11-10-2022

library(igraph)

################################################################################
#                                 CUMULATIVE
################################################################################

#PLOT
pdf(file="degree_distributions_1_CUMULATIVE.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 1:50){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_2_CUMULATIVE.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 51:100){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_3_CUMULATIVE.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 101:150){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_4_CUMULATIVE.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 151:200){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_5_CUMULATIVE.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 201:250){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_6_CUMULATIVE.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 251:300){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_7_CUMULATIVE.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 301:310){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()


################################################################################
#                           CUMULATIVE - LOG-LOG
################################################################################

#PLOT
pdf(file="degree_distributions_1_CUMULATIVE_LOG_LOG.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 1:50){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_2_CUMULATIVE_LOG_LOG.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 51:100){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net), log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency", log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency", log="xy")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_3_CUMULATIVE_LOG_LOG.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 101:150){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net), log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_4_CUMULATIVE_LOG_LOG.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 151:200){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net),log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_5_CUMULATIVE_LOG_LOG.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 201:250){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net), log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_6_CUMULATIVE_LOG_LOG.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 251:300){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net), log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_7_CUMULATIVE_LOG_LOG.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 301:310){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net), log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = T, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency",log="xy")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

