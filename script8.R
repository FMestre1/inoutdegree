################################################################################
# Area between the two curves
################################################################################
#FMestre
#19-12-2023

#library(tab)

names(in_degree_list)
names(out_degree_list)

in_degree_list_selected <- in_degree_list[names(in_degree_list) %in% final_data_frame_8_SPATIAL$network_number]
out_degree_list_selected <- out_degree_list[names(out_degree_list) %in% final_data_frame_8_SPATIAL$network_number]

#length(in_degree_list_selected)
#length(out_degree_list_selected)
#
#names(in_degree_list_selected)
#names(out_degree_list_selected)

#plot(mutualistic_networks_igraph[[110]])
#degree_distribution(mutualistic_networks_igraph[[110]], cumulative = TRUE, mode = "in")
#degree_distribution(mutualistic_networks_igraph[[110]], cumulative = TRUE, mode = "out")
#dev.off()

in_degree_list_selected_DF <- list()
out_degree_list_selected_DF <- list()

for(i in 1:length(in_degree_list_selected)){
  
df_in <- data.frame(0:(length(in_degree_list_selected[[i]])-1), in_degree_list_selected[[i]])
df_out <- data.frame(0:(length(out_degree_list_selected[[i]])-1), out_degree_list_selected[[i]])

names(df_in) <- c("nr_vertices", "relative_freq")
names(df_out) <- c("nr_vertices", "relative_freq")

in_degree_list_selected_DF[[i]] <- df_in
out_degree_list_selected_DF[[i]] <- df_out

}

names(in_degree_list_selected_DF) <- names(out_degree_list_selected)
names(out_degree_list_selected_DF) <- names(out_degree_list_selected)

dif_in_out <- c()

for(i in 1:length(in_degree_list_selected_DF)){
  
#Use this as an example
#plot(in_degree_list_selected_DF[[i]], type = "l", col = "green", lwd = 2)
#lines(out_degree_list_selected_DF[[i]], type = "l", col = "red", lwd = 2)


in_out_lines <- merge(in_degree_list_selected_DF[[i]],
                      out_degree_list_selected_DF[[i]],
                      by = "nr_vertices", 
                      all=TRUE
                      )

names(in_out_lines) <- c("degree", "in_freq", "out_freq")

dif_in_out[i] <- mean(in_out_lines$in_freq - in_out_lines$out_freq, na.rm = TRUE)

}

dif_in_out_DF <- data.frame(names(out_degree_list_selected), dif_in_out)

names(dif_in_out_DF) <- c("network_id", "average_difference")
head(dif_in_out_DF)


df_variables_difference <- data.frame(final_data_frame_8_SPATIAL)
df_variables_difference <- data.frame(df_variables_difference, dif_in_out_DF)
df_variables_difference <- df_variables_difference[,-c(33,35, 42)]
names(df_variables_difference)[33] <- "solar_radiation" 
names(df_variables_difference)[34] <- "human_footprint" 

df_variables_difference_MUT <- df_variables_difference[df_variables_difference$type == "mutualistic",]
df_variables_difference_ANT <- df_variables_difference[df_variables_difference$type == "antagonistic",]

glm_ANT <- glm(average_difference ~ ., data = df_variables_difference_ANT[,-c(1:13, 35:39)])
glm_MUT <- glm(average_difference ~ ., data = df_variables_difference_MUT[,-c(1:13, 35:39)])

summary(glm_ANT)
summary(glm_MUT)

#?tabglm

#tab::tabglm(glm_ANT, xvarlabels = list( bio1 = "Annual Mean Temperature", 
#                                        bio9 = "Mean Temperature of DriestQuarter", 
#                                        bio12 = "Annual Mean Specific Humidity", 
#                                        bio14 = "Precipitation of Driest Month", 
#                                        solar_radiation = "Solar radiation", 
#                                        human_footprint = "Human footprint"
#                                       )
#            )

