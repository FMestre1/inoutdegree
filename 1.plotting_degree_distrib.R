#FMestre
#03-10-2022

library(rmangal)
library(igraph)

#table(final_data_frame9$ecosystem)

#Retain these networks
final_data_frame9$network_number
retain_these_fw <- final_data_frame9[final_data_frame9$ecosystem != "marine",]$network_number
retain_these_fw2 <- c()
for(i in 1:length(retain_these_fw)){

retain_these_fw2 <- c(retain_these_fw2, as.numeric(strsplit(retain_these_fw[[i]], split = "#")[[1]][2])) 
  
}
################################################################################

all_mangal_objects <- combine_mgNetworks(mutualistic_networks, antagonistic_networks)
all_mangal_objects_selected <- NA
class(all_mangal_objects_selected) <- "mgNetworksCollection"

id_table_type <- data.frame(final_data_frame9$network_number,final_data_frame9$type,final_data_frame9$network_description)
for(i in 1:nrow(id_table_type)){
id_table_type[i,4] <- as.numeric(strsplit(id_table_type$final_data_frame9.network_number[[i]], split="#", fixed=T)[[1]][2])
}

#View(id_table_type)

for(i in 1:length(all_mangal_objects)){

if(all_mangal_objects[[i]]$network$network_id %in% retain_these_fw2) all_mangal_objects_selected <- combine_mgNetworks(all_mangal_objects_selected, all_mangal_objects[[i]])
  
}

all_mangal_objects_selected <- all_mangal_objects_selected[-1]
length(all_mangal_objects_selected)
class(all_mangal_objects_selected) <- "mgNetworksCollection"

all_mangal_objects_selected_igraph <- as.igraph(all_mangal_objects_selected)

#PLOT
pdf(file="degree_distributions_1.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 1:50){
    
type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type

plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)

}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_2.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 51:100){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_3.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 101:150){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_4.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 151:200){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_5.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 201:250){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_6.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 251:300){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

#############

#PLOT
pdf(file="degree_distributions_7.pdf",  width=15, height=150)

#par(mfrow=c(310,4), mar = c(3, 3, 3, 3))
par(mfrow=c(50,4), mar = c(3, 3, 3, 3))

#for(i in 1:length(all_mangal_objects_selected_igraph)){
for(i in 301:310){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F)), type="l", lwd=2, xlab = "nr. nodes", ylab = "frequency", main = paste0(i," - ", type_net))
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "in")), type="l", col = "red", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = F, mode = "out")), type="l", col = "blue", lwd=2, xlab = "nr. nodes", ylab = "frequency")
  plot(all_mangal_objects_selected_igraph[[i]], layout=layout_in_circle)
  
}
dev.off()

