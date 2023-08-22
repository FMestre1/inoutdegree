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



####

#Save to shapefile

names(final_data_frame_13)

final_data_frame_13_SPDF <- SpatialPointsDataFrame(coords = final_data_frame_13[,7:8], data = final_data_frame_13)
rgdal::writeOGR(obj=final_data_frame_13_SPDF, dsn="tempdir", layer="final_data_frame_13_SPDF", driver="ESRI Shapefile")

#nrow(final_data_frame_13_SPDF)
#table(final_data_frame_13_SPDF$type)

####

################################################################################
#                                   TREES
################################################################################

library(mvpart)

#Create matrix with response variables
responses_MUT <- cbind(final_data_frame_13_MUT$sq_wasserstein_in_out_location_PERC, final_data_frame_13_MUT$sq_wasserstein_in_out_size_PERC, final_data_frame_13_MUT$sq_wasserstein_in_out_shape_PERC)
responses_MUT[is.na(responses_MUT)] <- 0


# First, remove the "distance from source" variable
env <- subset(env, select = -das)


# Create multivariate regression tree
mvpart(
  responses_MUT ~ y+bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+ecosystem, 
  data = final_data_frame_13_MUT,
  xv = "min",
  xval = nrow(responses_MUT), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE,  # give "rsq" plot
  pca = TRUE,  # plot PCA of group means and add species and site information
  wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

##

#Create matrix with response variables
responses_FW <- cbind(final_data_frame_13_FW$sq_wasserstein_in_out_location_PERC, final_data_frame_13_FW$sq_wasserstein_in_out_size_PERC, final_data_frame_13_FW$sq_wasserstein_in_out_shape_PERC)
responses_FW[is.na(responses_FW)] <- 0 

names(final_data_frame_13_FW)

mvpart(
  responses_FW ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, 
  data = final_data_frame_13_FW,
  xv = "min",
  xval = nrow(responses_FW), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE,  # give "rsq" plot
  pca = TRUE,  # plot PCA of group means and add species and site information
  wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

################################################################################
# After de meeting...
################################################################################

#FMestre
#04-11-2022

library(igraph)

#Derive the average consumer/resource ratio for each network
#all_mangal_objects_selected_igraph

cr_ratio_vector <- c()

for(i in 1:length(all_mangal_objects_selected_igraph)){
t2 <- all_mangal_objects_selected_igraph[[i]]
nr_preys <- length(unique(as.numeric(as_edgelist(t2, names = TRUE)[,1])))#preys
nr_predators <- length(unique(as.numeric(as_edgelist(t2, names = TRUE)[,2])))#predators
cr_ratio_vector[i] <- nr_predators/nr_preys  

}

#length(cr_ratio_vector)

#Create another data frame with the CR ratio
final_data_frame_14 <- data.frame(final_data_frame_12, cr_ratio_vector)
final_data_frame_14 <- final_data_frame_14[final_data_frame_14$ecosystem != "marine",]
nrow(final_data_frame_14)

#Separate it per type of network 
final_data_frame_14_FW <- final_data_frame_14[final_data_frame_14$type=="antagonistic",]
final_data_frame_14_MUT <- final_data_frame_14[final_data_frame_14$type=="mutualistic",]

plot(final_data_frame_14_FW$cr_ratio_vector, final_data_frame_14_FW$h_foot_vector, xlab = "CR Ratio", ylab = "Human footprint")
plot(final_data_frame_14_FW$cr_ratio_vector, final_data_frame_14_FW$solar_radiation, xlab = "CR Ratio", ylab = "Solar radiation")

##

library(mvpart)

MUT_tree <- mvpart(
  responses_MUT ~ y+solar_radiation+h_foot_vector+ecosystem, 
  data = final_data_frame_14_MUT,
  xv = "min",
  xval = nrow(responses_MUT), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE#,  # give "rsq" plot
  #pca = TRUE,  # plot PCA of group means and add species and site information
  #wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

names(final_data_frame_14_FW)

FW_tree <- mvpart(
  responses_FW ~ y+solar_radiation+h_foot_vector, 
  data = final_data_frame_14_FW,
  xv = "min",
  xval = nrow(responses_FW), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE#,  # give "rsq" plot
  #pca = TRUE,  # plot PCA of group means and add species and site information
  #wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

MUT_tree_pruned <- prune(MUT_tree, cp=0.1)
FW_tree_pruned <- prune(FW_tree, cp=0.1)
#
library(rpart.plot)
#
rpart.plot::rpart.plot(FW_tree)
rpart.plot::rpart.plot(FW_tree_pruned)
rpart.plot::rpart.plot(MUT_tree)
rpart.plot::rpart.plot(MUT_tree_pruned)


################################################################################
# Creating trees with the overall distance only
################################################################################
#14-11-2022
#FMestre

sq_wasserstein_in_out2 <- data.frame(code_network,
                                    sq_wasserstein_in_out_distance, 
                                    sq_wasserstein_in_out_location_PERC,
                                    sq_wasserstein_in_out_size_PERC,
                                    sq_wasserstein_in_out_shape_PERC
)

retrieve_codes <- stringr::str_split(sq_wasserstein_in_out2$code_network, "_")

retrieve_codes2 <- c()

for(i in 1:length(retrieve_codes)){
  
  
  retrieve_codes2[i] <- retrieve_codes[[i]][2]
  
  
  
  
  
}

sq_wasserstein_in_out2 <- data.frame(retrieve_codes2,
                                     sq_wasserstein_in_out2
                                     )

sq_wasserstein_in_out2 <- data.frame(sq_wasserstein_in_out2$retrieve_codes2,
                                     sq_wasserstein_in_out2$sq_wasserstein_in_out_distance)

names(sq_wasserstein_in_out2) <- c("codes", "distance")

head(sq_wasserstein_in_out2)


final_data_frame_15 <- merge(x=final_data_frame_14, y=sq_wasserstein_in_out2, by.x = "network_number", by.y = "codes")
nrow(final_data_frame_15)
View(final_data_frame_15)


#Separate it per type of network 
final_data_frame_15_FW <- final_data_frame_15[final_data_frame_15$type=="antagonistic",]
final_data_frame_15_MUT <- final_data_frame_15[final_data_frame_15$type=="mutualistic",]


names(final_data_frame_15_FW)

#
library(ggplot2)

#plot(final_data_frame_15_MUT$cr_ratio_vector, final_data_frame_15_MUT$sq_wasserstein_in_out_location_PERC)

ggplot2::ggplot(final_data_frame_15_FW, aes(x=cr_ratio_vector, y=distance)) + 
  geom_point()+
  #stat_smooth(method = "lm", formula = y ~ x, size = 1) +
  xlab("Consumer-Resource Ratio") + ylab("Distance") +
  #theme(legend.position="none")
  theme_minimal() +
  #geom_smooth(formula= (y ~ x, se=TRUE, fullrange=FALSE, level=0.95)
  #stat_smooth(method = 'lm', formula = y ~ x, aes(colour = 'polynomial'), se= FALSE)
  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), 
            se = FALSE, start = list(a=1,b=1))
  


#

#final_data_frame_15_MUT[final_data_frame_15_MUT$cr_ratio_vector>4,]

ggplot2::ggplot(final_data_frame_15_MUT, aes(x=cr_ratio_vector, y=distance)) + 
  geom_point()+
  #stat_smooth(method = "lm", formula = y ~ x, size = 1) + 
  xlab("Consumer-Resource Ratio") + ylab("Distance") +
  #theme(legend.position="none")
  theme_minimal()+
  #geom_smooth(formula= (y ~ log(x)), se=TRUE, fullrange=TRUE, level=0.95)
  #stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE)
  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), 
              se = FALSE, start = list(a=1,b=1))

#lm1 <- lm(cr_ratio_vector~distance, data = final_data_frame_15_MUT)
#summary(lm1)

################################################################################

final_data_frame_15_FW_2 <- final_data_frame_15_FW[,c("distance", "bio1", "bio4", "bio12", "bio15", "solar_radiation", "h_foot_vector", "y")]
names(final_data_frame_15_FW_2)[6] <- "solar_radiation"
names(final_data_frame_15_FW_2)[7] <- "human_footprint"
names(final_data_frame_15_FW_2)[8] <- "latitude"
#
final_data_frame_15_MUT_2 <- final_data_frame_15_MUT[,c("distance", "bio1", "bio4", "bio12", "bio15", "solar_radiation", "h_foot_vector", "y")]
names(final_data_frame_15_MUT_2)[6] <- "solar_radiation"
names(final_data_frame_15_MUT_2)[7] <- "human_footprint"
names(final_data_frame_15_MUT_2)[8] <- "latitude"
#

rpart_FW_3 <- rpart::rpart(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint+latitude, 
                    data = final_data_frame_15_FW_2)
#FW_tree_pruned_3 <- prune(rpart_FW_3, cp=0.1)
#rpart.plot::rpart.plot(FW_tree_pruned_3)
rpart.plot::rpart.plot(rpart_FW_3)

#

rpart_MUT_3 <- rpart::rpart(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint+latitude, 
                           data = final_data_frame_15_MUT_2)
#MUT_tree_pruned_3 <- prune(rpart_MUT_3, cp=0.1)
#rpart.plot::rpart.plot(MUT_tree_pruned_3)
rpart.plot::rpart.plot(rpart_MUT_3)

##REcovering multivariate trees with 3 distances

library(mvpart)

final_data_frame_14_MUT_2 <- final_data_frame_14_MUT[,c("bio1", "bio4", "bio12", "bio15", "solar_radiation", "h_foot_vector", "y")]
names(final_data_frame_14_MUT_2)[6] <- "human_footprint"
names(final_data_frame_14_MUT_2)[7] <- "latitude"

MUT_tree <- mvpart(
  responses_MUT ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint+latitude, 
  data = final_data_frame_14_MUT_2,
  xv = "min",
  xval = nrow(responses_MUT), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE#,  # give "rsq" plot
  #pca = TRUE,  # plot PCA of group means and add species and site information
  #wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)


final_data_frame_14_FW_2 <- final_data_frame_14_FW[,c("bio1", "bio4", "bio12", "bio15", "solar_radiation", "h_foot_vector", "y")]
names(final_data_frame_14_FW_2)[6] <- "human_footprint"
names(final_data_frame_14_FW_2)[7] <- "latitude"

FW_tree <- mvpart(
  responses_FW ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint+latitude, 
  data = final_data_frame_14_FW_2,
  xv = "min",
  xval = nrow(responses_FW), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE#,  # give "rsq" plot
  #pca = TRUE,  # plot PCA of group means and add species and site information
  #wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)



library(rpart.plot)
#3 distances
#rpart.plot(MUT_tree, box.palette="orange4", shadow.col="gray", nn=TRUE)
#rpart.plot(FW_tree, box.palette="yellow4", shadow.col="gray", nn=TRUE)

#overall distance
rpart.plot(rpart_MUT_3, box.palette="orange3", shadow.col="gray", nn=TRUE)
rpart.plot(rpart_FW_3, box.palette="yellow3", shadow.col="gray", nn=TRUE)


###
#21-11-2022

library(dplyr)       #for data wrangling
library(e1071)       #for calculating variable importance
library(caret)       #for general model fitting
library(rpart)       #for fitting decision trees
library(ipred)       #for fitting bagged decision trees


bag_FW <- bagging(
  formula = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint+latitude,
  data = final_data_frame_15_FW_2,
  nbagg = nrow(final_data_frame_15_FW_2),   
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

bag_FW

#calculate variable importance
VI_fw <- varImp(bag_FW)
VI_fw <- data.frame(var=row.names(VI_fw), imp=VI_fw[,1])

#sort variable importance descending
VI_plot_fw <- VI_fw[order(VI_fw$imp, decreasing=TRUE),]

#visualize variable importance with horizontal bar plot
barplot(VI_plot_fw$imp,
        names.arg=VI_plot_fw$var,
        horiz=TRUE,
        col='steelblue',
        xlab='Variable Importance')

#####

bag_MUT <- bagging(
  formula = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint+latitude,
  data = final_data_frame_15_MUT_2,
  nbagg = nrow(final_data_frame_15_MUT_2),   
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

bag_MUT

#calculate variable importance
VI_mu <- varImp(bag_MUT)
VI_mu <- data.frame(var=row.names(VI_mu), imp=VI_mu[,1])

#sort variable importance descending
VI_plot_mu <- VI_mu[order(VI_mu$imp, decreasing=TRUE),]

#visualize variable importance with horizontal bar plot
barplot(VI_plot_mu$imp,
        names.arg=VI_plot_mu$var,
        horiz=TRUE,
        col='steelblue',
        xlab='Variable Importance')


################################################################################
#             GENERALITY & VULNERABILITY VS LATITUDE
################################################################################
#FMestre
#21-11-2022

length(final_data_frame_14_MUT$network_number)
length(final_data_frame_14_FW$network_number)

retain_id <- c(final_data_frame_14_MUT$network_number, final_data_frame_14_FW$network_number)
retain_id <- stringr::str_split(retain_id, "#")

retain_id_2 <- c()

for(i in 1:length(retain_id)) retain_id_2[i] <- as.numeric(retain_id[[i]][2])

#
nrow(final_data_frame_15_MUT_2) + nrow(final_data_frame_15_FW_2)
length(all_mangal_objects_selected_igraph)
length(all_mangal_objects_selected)

#all_mangal_objects_selected_igraph <- as.igraph(all_mangal_objects_selected)

all_mangal_objects_selected_igraph_RETAINED <- list()

#all_mangal_objects_selected[[1]]$network$network_id

for(i in 1:length(retain_id_2)){
  
  id_1 <- retain_id_2[i]
  
  for(j in 1:length(all_mangal_objects_selected)){
    
    #if(all_mangal_objects_selected[[j]]$network$network_id == id_1) all_mangal_objects_selected_igraph_RETAINED[[j]] <- all_mangal_objects_selected_igraph[[j]]
    #if(all_mangal_objects_selected[[j]]$network$network_id == id_1) all_mangal_objects_selected_igraph_RETAINED <- c(all_mangal_objects_selected_igraph_RETAINED, all_mangal_objects_selected_igraph[[j]])
    if(all_mangal_objects_selected[[j]]$network$network_id == id_1) all_mangal_objects_selected_igraph_RETAINED[[length(all_mangal_objects_selected_igraph_RETAINED)+1]] <- all_mangal_objects_selected_igraph[[j]]
    
    
     }

} 

length(all_mangal_objects_selected_igraph_RETAINED)
length(all_mangal_objects_selected)
length(retain_id_2)

vuln <- c()
gen <- c()

library(igraph)

for(i in 1:length(all_mangal_objects_selected_igraph_RETAINED)){
  t3 <- all_mangal_objects_selected_igraph_RETAINED[[i]]
  nr_preys <- length(unique(as.numeric(as_edgelist(t3, names = TRUE)[,1])))#preys
  nr_predators <- length(unique(as.numeric(as_edgelist(t3, names = TRUE)[,2])))#predators
  
  vuln[i] <-  nr_predators/nr_preys
  gen[i] <-  nr_preys/nr_predators
  
}

final_data_frame_16_MUT <- data.frame(final_data_frame_14_MUT, vuln[1:152], gen[1:152])
final_data_frame_16_FW <- data.frame(final_data_frame_14_FW, vuln[153:296], gen[153:296])
#


#PLOT

#use latitude as absolute value

final_data_frame_16_MUT$abs_lat <- abs(final_data_frame_16_MUT$y)
final_data_frame_16_FW$abs_lat <- abs(final_data_frame_16_FW$y)

#Vulnerability - Mutualistic networks
ggplot2::ggplot(final_data_frame_16_MUT, aes(x=abs_lat, y=vuln.1.152.)) + 
  geom_point()+
  xlab("Latitude") + ylab("Vulnerability") + ggtitle("Mutualistic networks") +
  theme_minimal() 
  #+ stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))

#Vulnerability - Antagonistic networks
ggplot2::ggplot(final_data_frame_16_FW, aes(x=abs_lat, y=vuln.153.296.)) + 
  geom_point()+
  xlab("Latitude") + ylab("Vulnerability") + ggtitle("Antagonistic networks") +
  theme_minimal()
  #+ stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))

#Generality - Mutualistic networks
ggplot2::ggplot(final_data_frame_16_MUT, aes(x=abs_lat, y=gen.1.152.)) + 
  geom_point()+
  xlab("Latitude") + ylab("Generality") + ggtitle("Mutualistic networks") +
  theme_minimal() 
#+ stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))

#Generality - Antagonistic networks
ggplot2::ggplot(final_data_frame_16_FW, aes(x=abs_lat, y=gen.153.296.)) + 
  geom_point()+
  xlab("Latitude") + ylab("Generality") + ggtitle("Antagonistic networks") +
  theme_minimal()
#+ stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))

