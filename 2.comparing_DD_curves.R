#Comparing Degree Distribution Curves

#FMestre
#20-10-2022

#loading package
library(waddR)
library(igraph)

#data I have currently
all_mangal_objects_selected_igraph
all_mangal_objects_selected
#
#length(all_mangal_objects_selected_igraph)
#length(all_mangal_objects_selected)

overall_degree_distribution_list <- list()
in_degree_distribution_list <- list()
out_degree_distribution_list <- list()
code_network <- c()

for(i in 1:310){
  
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  type_net <- id_table_type[id_table_type$V4 == all_mangal_objects_selected[[i]]$network$network_id,]$final_data_frame9.type
  code_network[i] <- paste0(id_table_type$final_data_frame9.type[i], "_",  id_table_type$final_data_frame9.network_number[i])
  
  overall_degree_distribution_list[[i]] <- as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = TRUE))
  in_degree_distribution_list[[i]] <- as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = TRUE, mode = "in"))
  out_degree_distribution_list[[i]] <- as.numeric(degree_distribution(all_mangal_objects_selected_igraph[[i]], cumulative = TRUE, mode = "out"))

message("Concluded ",i)
  
}

names(overall_degree_distribution_list) <- code_network
names(in_degree_distribution_list) <- code_network
names(out_degree_distribution_list) <- code_network

#as.numeric(lapply(overall_degree_distribution_list, length))
#as.numeric(lapply(in_degree_distribution_list, length))
#as.numeric(lapply(out_degree_distribution_list, length))

##########################################################################################
# How different are in and out degree distributions?
##########################################################################################

sq_wasserstein_in_out_distance <- c()
sq_wasserstein_in_out_location <- c()
sq_wasserstein_in_out_size <- c()
sq_wasserstein_in_out_shape <- c()

for(i in 1:310) {
  
sq_wasserstein_in_out_distance[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], out_degree_distribution_list[[i]])[1])
sq_wasserstein_in_out_location[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], out_degree_distribution_list[[i]])[2])
sq_wasserstein_in_out_size[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], out_degree_distribution_list[[i]])[3])
sq_wasserstein_in_out_shape[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], out_degree_distribution_list[[i]])[4])

}

#Derive percentages
sq_wasserstein_in_out_location_PERC <- (sq_wasserstein_in_out_location*100)/sq_wasserstein_in_out_distance
sq_wasserstein_in_out_size_PERC <- (sq_wasserstein_in_out_size*100)/sq_wasserstein_in_out_distance
sq_wasserstein_in_out_shape_PERC <- (sq_wasserstein_in_out_shape*100)/sq_wasserstein_in_out_distance


#Sum up to 100?
sq_wasserstein_in_out_location_PERC + sq_wasserstein_in_out_size_PERC + sq_wasserstein_in_out_shape_PERC
#Those that don't are all 0
#sq_wasserstein_in_out_distance[81]
#sq_wasserstein_in_out_location[81]
#sq_wasserstein_in_out_size[81]
#sq_wasserstein_in_out_shape[81]

sq_wasserstein_in_out <- data.frame(code_network, 
                                    sq_wasserstein_in_out_location_PERC,
                                    sq_wasserstein_in_out_size_PERC,
                                    sq_wasserstein_in_out_shape_PERC
                                    )

View(sq_wasserstein_in_out)


fact1 <- c(rep("location", 310), rep("size", 310), rep("shape", 310))
in_out_vector <- c(sq_wasserstein_in_out_location_PERC, sq_wasserstein_in_out_size_PERC, sq_wasserstein_in_out_shape_PERC)
df_in_out_vect <- data.frame(fact1, in_out_vector)

boxplot(in_out_vector ~ fact1)

library(beanplot)

beanplot(in_out_vector ~ fact1)

##########################################################################################
# How different are in and out degree distributions from the overall degree distribution?
##########################################################################################

sq_wasserstein_in_overall_distance <- c()
sq_wasserstein_in_overall_location <- c()
sq_wasserstein_in_overall_size <- c()
sq_wasserstein_in_overall_shape <- c()
#
sq_wasserstein_out_overall_distance <- c()
sq_wasserstein_out_overall_location <- c()
sq_wasserstein_out_overall_size <- c()
sq_wasserstein_out_overall_shape <- c()

for(i in 1:310) {
  
sq_wasserstein_in_overall_distance[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[1])
sq_wasserstein_in_overall_location[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[2])
sq_wasserstein_in_overall_size[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[3])
sq_wasserstein_in_overall_shape[i] <- as.numeric(squared_wass_decomp(in_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[4])
#
sq_wasserstein_out_overall_distance[i] <- as.numeric(squared_wass_decomp(out_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[1])
sq_wasserstein_out_overall_location[i] <- as.numeric(squared_wass_decomp(out_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[2])
sq_wasserstein_out_overall_size[i] <- as.numeric(squared_wass_decomp(out_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[3])
sq_wasserstein_out_overall_shape[i] <- as.numeric(squared_wass_decomp(out_degree_distribution_list[[i]], overall_degree_distribution_list[[i]])[4])
  
}

#Derive percentages
sq_wasserstein_in_overall_location_PERC <- (sq_wasserstein_in_overall_location*100)/sq_wasserstein_in_overall_distance
sq_wasserstein_in_overall_size_PERC <- (sq_wasserstein_in_overall_size*100)/sq_wasserstein_in_overall_distance
sq_wasserstein_in_overall_shape_PERC <- (sq_wasserstein_in_overall_shape*100)/sq_wasserstein_in_overall_distance
#
sq_wasserstein_out_overall_location_PERC <- (sq_wasserstein_out_overall_location*100)/sq_wasserstein_out_overall_distance
sq_wasserstein_out_overall_size_PERC <- (sq_wasserstein_out_overall_size*100)/sq_wasserstein_out_overall_distance
sq_wasserstein_out_overall_shape_PERC <- (sq_wasserstein_out_overall_shape*100)/sq_wasserstein_out_overall_distance
#

#Sum up to 100?
#sq_wasserstein_in_overall_location_PERC + sq_wasserstein_in_overall_size_PERC + sq_wasserstein_in_overall_shape_PERC
#sq_wasserstein_out_overall_location_PERC + sq_wasserstein_out_overall_size_PERC + sq_wasserstein_out_overall_shape_PERC


sq_wasserstein_in_OVERALL <- data.frame(code_network, 
                                            sq_wasserstein_in_overall_location_PERC,
                                            sq_wasserstein_in_overall_size_PERC,
                                            sq_wasserstein_in_overall_shape_PERC
                                            )

sq_wasserstein_out_OVERALL <- data.frame(code_network, 
                                            sq_wasserstein_out_overall_location_PERC,
                                            sq_wasserstein_out_overall_size_PERC,
                                            sq_wasserstein_out_overall_shape_PERC
                                            )

View(sq_wasserstein_in_out_OVERALL)

#in - overall
fact1 <- c(rep("location", 310), rep("size", 310), rep("shape", 310))
in_overall_vector <- c(sq_wasserstein_in_overall_location_PERC, sq_wasserstein_in_overall_size_PERC, sq_wasserstein_in_overall_shape_PERC)
df_in_overall_vect <- data.frame(fact1, in_overall_vector)

boxplot(in_overall_vector ~ fact1)

library(beanplot)

beanplot(in_overall_vector ~ fact1)

#out - overall
fact1 <- c(rep("location", 310), rep("size", 310), rep("shape", 310))
out_overall_vector <- c(sq_wasserstein_out_overall_location_PERC, sq_wasserstein_out_overall_size_PERC, sq_wasserstein_out_overall_shape_PERC)
df_out_overall_vect <- data.frame(fact1, out_overall_vector)

boxplot(out_overall_vector ~ fact1)

library(beanplot)

beanplot(out_overall_vector ~ fact1)

##########################################################################################
# Combine this with previous information
##########################################################################################

code1 <- stringr::str_split(sq_wasserstein_in_out_OVERALL$code_network, "_")

code2 <- c()

for(i in 1:length(code1)){

code2[i] <-  code1[[i]][2]
  
}

get_code_to_select_from_df9 <- code2

final_data_frame_10 <- final_data_frame9[final_data_frame9$network_number %in% get_code_to_select_from_df9,]
nrow(final_data_frame_10)

final_data_frame_10$network_number
sq_wasserstein_in_out$code_network
sq_wasserstein_in_out_OVERALL$code_network

length(final_data_frame9$network_number)
length(sq_wasserstein_in_out$code_network)
length(sq_wasserstein_in_out_OVERALL$code_network)

final_data_frame_11 <- data.frame(final_data_frame_10, 
                                  sq_wasserstein_in_out$sq_wasserstein_in_out, 
                                  sq_wasserstein_in_out_OVERALL$sq_wasserstein_in_overall, 
                                  sq_wasserstein_in_out_OVERALL$sq_wasserstein_out_overall
                                  )
View(final_data_frame_11)

#Separate per type of network 
final_data_frame_11_FW <- final_data_frame_11[final_data_frame_11$type=="antagonistic",]
final_data_frame_11_MUT <- final_data_frame_11[final_data_frame_11$type=="mutualistic",]

#nrow(final_data_frame_11_FW)
#nrow(final_data_frame_11_MUT)

#Boxplot
boxplot(final_data_frame_11$sq_wasserstein_in_out.sq_wasserstein_in_out ~ final_data_frame_11$type)
boxplot(final_data_frame_11$sq_wasserstein_in_out.sq_wasserstein_in_out ~ final_data_frame_11$ecosystem)
boxplot(final_data_frame_11_FW$sq_wasserstein_in_out.sq_wasserstein_in_out ~ final_data_frame_11_FW$ecosystem)
boxplot(final_data_frame_11_MUT$sq_wasserstein_in_out.sq_wasserstein_in_out ~ final_data_frame_11_MUT$ecosystem)

#Save tables
save(final_data_frame_11, file = "final_data_frame_11.RData")
save(final_data_frame_11_FW, file =  "final_data_frame_11_FW.RData")
save(final_data_frame_11_MUT, file =  "final_data_frame_11_MUT.RData")


####################################################################################
#
####################################################################################

#Update final data frame 11 with information from the lines 99 - 173

final_data_frame_12 <- data.frame(final_data_frame_10, 
                                  sq_wasserstein_in_OVERALL,
                                  sq_wasserstein_out_OVERALL,
                                  sq_wasserstein_in_out
)

View(final_data_frame_12)

#final_data_frame_12$network_number
#final_data_frame_12$type
#final_data_frame_12$code_network.1
#final_data_frame_12$code_network.2
#final_data_frame_12$code_network

#Separate per type of network 
final_data_frame_12_FW <- final_data_frame_12[final_data_frame_12$type=="antagonistic",]
final_data_frame_12_MUT <- final_data_frame_12[final_data_frame_12$type=="mutualistic",]

#Save tables
save(final_data_frame_12, file = "final_data_frame_12.RData")
save(final_data_frame_12_FW, file =  "final_data_frame_12_FW.RData")
save(final_data_frame_12_MUT, file =  "final_data_frame_12_MUT.RData")

#Explore relationships
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_in_overall_location_PERC)
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_in_overall_size_PERC)
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_in_overall_shape_PERC)
#
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_out_overall_location_PERC)
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_out_overall_size_PERC)
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_out_overall_shape_PERC)
#
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_in_out_location_PERC)
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_in_out_size_PERC)
plot(final_data_frame_12_FW$h_foot_vector,final_data_frame_12_FW$sq_wasserstein_in_out_shape_PERC)


##Adding distance
final_data_frame_13 <- data.frame(final_data_frame_12, sq_wasserstein_in_out_distance)
final_data_frame_13 <- final_data_frame_13[final_data_frame_13$ecosystem != "marine",]


#Separate per type of network 
final_data_frame_13_FW <- final_data_frame_13[final_data_frame_13$type=="antagonistic",]
final_data_frame_13_MUT <- final_data_frame_13[final_data_frame_13$type=="mutualistic",]


boxplot(final_data_frame_13$sq_wasserstein_in_out_distance~final_data_frame_13$type)

aov1 <- aov(final_data_frame_13$sq_wasserstein_in_out_distance~final_data_frame_13$type)

summary(aov1)

#Explore relationships
plot(final_data_frame_13_FW$h_foot_vector,final_data_frame_13_FW$sq_wasserstein_in_out_distance)
plot(final_data_frame_13_MUT$h_foot_vector,final_data_frame_13_MUT$sq_wasserstein_in_out_distance)

#Code Vinicius
#27-10-2022
require(vegan)
bio=final_data_frame_13_MUT[,38:56]
pca=rda(bio)
biplot(pca, scaling = "symmetric", type = c("text", "points"))
summary(pca)
pca.scores=scores(pca)

#extracting values per site
pca.networks=pca.scores$sites

#extracting first axes per site
PCA1=pca.networks[,1];PCA1
PCA2=pca.networks[,2];PCA2

####Modeling mutualistic networks
require(nlme)

mod.lme_MUT=lm(sq_wasserstein_in_out_distance~h_foot_vector+solar_radiation+PCA1+PCA2+y,data=final_data_frame_13_MUT, na.action=na.omit)
summary(mod.lme_MUT)
plot_model(mod.lme_MUT)
#extract  coefficients
coef(mod.lme_MUT)


library(parameters)

#generate a html of lme results
tab_model(mod.lme, digits=2,file = "assortativity_index_mutualism.html")

#plot
result=model_parameters(mod.lme_MUT)
jpeg(file="assortativity_index_mutualism.jpeg")
plot(result)

#END
##

################################################################################
# TREES
################################################################################

library(rpart)  ######

names(final_data_frame_13_MUT)

rpart_MUT <- rpart(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, data = final_data_frame_13_MUT)

library("rpart.plot") ######

rpart.plot::rpart.plot(rpart_MUT)
summary(rpart_MUT)


rpart_FW <- rpart(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, data = final_data_frame_13_FW)

rpart.plot::rpart.plot(rpart_FW)
summary(rpart_FW)



library(party) ######

rpart_MUT_2 <- ctree(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, 
      data = final_data_frame_13_MUT
      )
plot(rpart_MUT_2)


rpart_FW_2 <- ctree(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, 
                     data = final_data_frame_13_FW
)
plot(rpart_FW_2)


library(mvpart) ######

#Create matrix with response variables
responses_MUT <- cbind(final_data_frame_13_MUT$sq_wasserstein_in_out_location_PERC, final_data_frame_13_MUT$sq_wasserstein_in_out_size_PERC, final_data_frame_13_MUT$sq_wasserstein_in_out_shape_PERC)
responses_MUT[is.na(responses_MUT)] <- 0 

mvpart_MUT1 <- mvpart(
  responses_MUT ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, 
  data = final_data_frame_13_MUT,
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE,  # give "rsq" plot
  pca = TRUE,  # plot PCA of group means and add species and site information
  wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

##

#Create matrix with response variables
responses_FW <- cbind(final_data_frame_13_FW$sq_wasserstein_in_out_location_PERC, final_data_frame_13_FW$sq_wasserstein_in_out_size_PERC, final_data_frame_13_FW$sq_wasserstein_in_out_shape_PERC)
responses_FW[is.na(responses_FW)] <- 0 

mvpart_FW1 <- mvpart(
  responses_FW ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, 
  data = final_data_frame_13_FW,
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE,  # give "rsq" plot
  pca = TRUE,  # plot PCA of group means and add species and site information
  wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)


####
#Save to shapefile

names(final_data_frame_13)

final_data_frame_13_SPDF <- SpatialPointsDataFrame(coords = final_data_frame_13[,7:8], data = final_data_frame_13)
rgdal::writeOGR(obj=final_data_frame_13_SPDF, dsn="tempdir", layer="final_data_frame_13_SPDF", driver="ESRI Shapefile")

nrow(final_data_frame_13_SPDF)
