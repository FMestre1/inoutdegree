################################################################################
#NEW PLOT IN-OUT DEGREE PER SPECIES COLOUR CODED BY DISTANCE AND CONS/RES RATIO
################################################################################

#FMestre
#24-02-2023

#all_mangal_objects_selected
#all_mangal_objects_selected_igraph
#retrieve_codes2

library(ggplot2)

all_species <- c()

for(i in 1:length(all_mangal_objects_selected_igraph)){
  
all_species <- c(all_species, vertex_attr(all_mangal_objects_selected_igraph[[i]])$taxonomy.name)
  
}

output_new_plot <- data.frame(NA, NA, NA, NA, NA, NA)
colnames(output_new_plot) <- c("species", "network", "in", "out", "distance", "CRratio")
head(output_new_plot)

for(i in 1:length(all_mangal_objects_selected_igraph)){
  species_n <- length(vertex_attr(all_mangal_objects_selected_igraph[[i]])$taxonomy.name)
  
  df123 <- data.frame(
   vertex_attr(all_mangal_objects_selected_igraph[[i]])$taxonomy.name, #species
   rep(names(in_degree_distribution_list)[i], species_n),#network
   as.numeric(degree(all_mangal_objects_selected_igraph[[i]], mode = "in")), #in
   as.numeric(degree(all_mangal_objects_selected_igraph[[i]], mode = "out")), #out
   rep(sq_wasserstein_in_out_distance[i], species_n), #distance
   rep(cr_ratio_vector[i], species_n) #CRratio
  )
  
  colnames(df123) <- c("species", "network", "in", "out", "distance", "CRratio")
  
  output_new_plot <- rbind(output_new_plot, df123)
  
  message(i)
  
}

output_new_plot_ANT <- output_new_plot[stringr::str_count(output_new_plot$network, "antagonistic") == 1,] 
output_new_plot_MUT <- output_new_plot[stringr::str_count(output_new_plot$network, "mutualistic") == 1,] 


plot(output_new_plot_ANT$`in`, output_new_plot_ANT$out)
plot(output_new_plot_MUT$`in`, output_new_plot_MUT$out)

names(output_new_plot_ANT)[3:4] <- c("in_deg", "out_deg")
names(output_new_plot_MUT)[3:4] <- c("in_deg", "out_deg")


ggplot(output_new_plot_ANT, aes(x=in_deg, y=out_deg, color=distance)) + 
  geom_point(alpha=0.3, size=3) +
  labs(y="out-degree", x="in-degree", subtitle="Antagonistic networks - Distance") +
  scale_color_gradient(low="yellow", high="blue")

ggplot(output_new_plot_MUT, aes(x=in_deg, y=out_deg, color=distance)) + 
  geom_point(alpha=0.3, size=3) +
  labs(y="out-degree", x="in-degree", subtitle="Mutualistic networks - Distance") +
  scale_color_gradient(low="yellow", high="blue")

ggplot(output_new_plot_ANT, aes(x=in_deg, y=out_deg, color=CRratio)) + 
  geom_point(alpha=0.3, size=3) +
  labs(y="out-degree", x="in-degree", subtitle="Antagonistic networks - CR Ration") +
  scale_color_gradient(low="yellow", high="blue")

ggplot(output_new_plot_MUT, aes(x=in_deg, y=out_deg, color=CRratio)) + 
  geom_point(alpha=0.3, size=3) +
  labs(y="out-degree", x="in-degree", subtitle="Mutualistic networks - CR Ration") +
  scale_color_gradient(low="yellow", high="blue")



################################################################################
#Test the relation between latitude and the alpha parameter for in and out
################################################################################

#FMestre
#07-03-2023


m1 <- glm(final_data_frame_16_MUT$alpha_in ~ abs(final_data_frame_16_MUT$y))
m2 <- glm(final_data_frame_16_MUT$alpha_out ~ abs(final_data_frame_16_MUT$y))
#
m3 <- glm(final_data_frame_16_FW$alpha_in ~ abs(final_data_frame_16_FW$y))
m4 <- glm(final_data_frame_16_FW$alpha_out ~ abs(final_data_frame_16_FW$y))

summary(m1)
summary(m2)
summary(m3)
summary(m4)

#Plot
plot(abs(final_data_frame_16_MUT$y), final_data_frame_16_MUT$alpha_in)
abline(m1)
