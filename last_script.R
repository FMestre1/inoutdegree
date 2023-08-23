################################################################################
#
################################################################################

#loading package
library(waddR)
library(igraph)
library(beanplot)
library(rmangal)
library(igraph)
library(sp)
library(rgl)
library(vegan)
library(mvpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(e1071)
library(caret)
library(rpart)
library(ipred)
library(bbmle)
library(minpack.lm)

################################################################################
#
################################################################################

#FMestre
#16-03-2022

#"mutualism", "predation", "herbivory", "scavenger", "detritivore"

mangal_mutualism <- search_interactions(type = "mutualism", verbose = TRUE)
mangal_predation <- search_interactions(type = "predation", verbose = TRUE)
mangal_herbivory <- search_interactions(type = "herbivory", verbose = TRUE)

mutualistic_networks <- get_collection(mangal_mutualism, as_sf = TRUE, verbose = TRUE)
#
mangal_collection_predation <- get_collection(mangal_predation, as_sf = TRUE, verbose = TRUE)
mangal_collection_herbivory <- get_collection(mangal_herbivory, as_sf = TRUE, verbose = TRUE)
antagonistic_networks <- combine_mgNetworks(mangal_collection_predation, mangal_collection_herbivory)
antagonistic_networks <- unique(antagonistic_networks)

#Save
save(mutualistic_networks, file="mutualistic_networks.RData")
save(antagonistic_networks, file="antagonistic_networks.RData")

#Convert to igraph
mutualistic_networks_igraph <- as.igraph(mutualistic_networks)
class(antagonistic_networks) <- class(mutualistic_networks)
antagonistic_networks_igraph <- as.igraph(antagonistic_networks)
#
save(mutualistic_networks_igraph, file="mutualistic_networks_igraph.RData")
save(antagonistic_networks_igraph, file="antagonistic_networks_igraph.RData")

#Getting the "in" and "out" degree of all networks
in_degree_list_MUT <- list()
out_degree_list_MUT <- list()
#
in_degree_list_ANT <- list()
out_degree_list_ANT <- list()
#
for(i in 1:length(mutualistic_networks_igraph)){
  
  in_degree_list_MUT[[i]] <- degree(mutualistic_networks_igraph[[i]], mode="in")
  out_degree_list_MUT[[i]] <- degree(mutualistic_networks_igraph[[i]], mode="out")
  
  message(paste0("Concluded matrix ", i, "!"))
  
}
#
for(i in 1:length(antagonistic_networks_igraph)){
  
  in_degree_list_ANT[[i]] <- degree(antagonistic_networks_igraph[[i]], mode="in")
  out_degree_list_ANT[[i]] <- degree(antagonistic_networks_igraph[[i]], mode="out")
  
  message(paste0("Concluded matrix ", i, "!"))
  
}

in_degree_list_2_MUT <- list()
out_degree_list_2_MUT <- list()
#
in_degree_list_2_ANT <- list()
out_degree_list_2_ANT <- list()
#
for(i in 1:length(mutualistic_networks_igraph)){
  
  in_degree_list_2_MUT[[i]] <- as.numeric(degree_distribution(mutualistic_networks_igraph[[i]], cumulative = TRUE, mode = "in"))
  out_degree_list_2_MUT[[i]] <- as.numeric(degree_distribution(mutualistic_networks_igraph[[i]], cumulative = TRUE, mode = "out"))
  
  message(paste0("Concluded matrix ", i, "!"))
  
}
#
for(i in 1:length(antagonistic_networks_igraph)){
  
  in_degree_list_2_ANT[[i]] <- as.numeric(degree_distribution(antagonistic_networks_igraph[[i]], cumulative = TRUE, mode = "in"))
  out_degree_list_2_ANT[[i]] <- as.numeric(degree_distribution(antagonistic_networks_igraph[[i]], cumulative = TRUE, mode = "out"))
  
  message(paste0("Concluded matrix ", i, "!"))
  
}

alpha_in_MUT <- c()
alpha_out_MUT <- c()
p_in_MUT <- c()
p_out_MUT <- c()
log_lik_in_MUT <- c()
log_lik_out_MUT <- c()
#
alpha_in_ANT <- c()
alpha_out_ANT <- c()
p_in_ANT <- c()
p_out_ANT <- c()
log_lik_in_ANT <- c()
log_lik_out_ANT <- c()

for(i in 1:length(mutualistic_networks_igraph)){
  
  fit_in <- fit_power_law(in_degree_list_2_MUT[[i]]+1)
  fit_out <- fit_power_law(out_degree_list_2_MUT[[i]]+1)
  
  alpha_in_MUT[i] <- fit_in$alpha
  alpha_out_MUT[i] <- fit_out$alpha
  p_in_MUT[i] <- fit_in$KS.p
  p_out_MUT[i] <- fit_out$KS.p
  log_lik_in_MUT[i] <- fit_in$logLik
  log_lik_out_MUT[i] <- fit_out$logLik
  
  message(paste0("Network ", i))
  
}

for(i in 1:length(antagonistic_networks_igraph)){
  
  fit_in <- fit_power_law(in_degree_list_2_ANT[[i]]+1)
  fit_out <- fit_power_law(out_degree_list_2_ANT[[i]]+1)
  
  
  alpha_in_ANT[i] <- fit_in$alpha
  alpha_out_ANT[i] <- fit_out$alpha
  p_in_ANT[i] <- fit_in$KS.p
  p_out_ANT[i] <- fit_out$KS.p
  log_lik_in_ANT[i] <- fit_in$logLik
  log_lik_out_ANT[i] <- fit_out$logLik
  
  message(paste0("Network ", i))
  
}

#Combine
type_MUT <- rep("mutualistic",length(mutualistic_networks_igraph))
type_ANT <- rep("antagonistic",length(antagonistic_networks_igraph))
type <- c(type_MUT, type_ANT)

alpha_in <- c(alpha_in_MUT, alpha_in_ANT)
alpha_out <- c(alpha_out_MUT, alpha_out_ANT)
p_in <- c(p_in_MUT, p_in_ANT)
p_out <- c(p_out_MUT, p_out_ANT)
log_lik_in <- c(log_lik_in_MUT, log_lik_in_ANT)
log_lik_out <- c(log_lik_out_MUT, log_lik_out_ANT)

###Data frame with in and out degree distributiin fit ##########################

fit_data_frame <- data.frame(
  type,
  alpha_in,
  alpha_out,
  p_in,
  p_out,
  log_lik_in,
  log_lik_out
)

nnodes_MUT <- c()
nedges_MUT <- c()
connectance_MUT <- c()
linkage_density_MUT <- c()
doi_MUT <- c()
year_MUT <- c()
first_author_MUT <- c()
paper_url_MUT <- c()
data_url_MUT <- c()
#
nnodes_ANT <- c()
nedges_ANT <- c()
connectance_ANT <- c()
linkage_density_ANT <- c()
doi_ANT <- c()
year_ANT <- c()
first_author_ANT <- c()
paper_url_ANT <- c()
data_url_ANT <- c()

for(i in 1:length(mutualistic_networks)){
  
  mangal1 <- mutualistic_networks[[i]]
  try({ mangal1_summ <- summary(mangal1)})
  
  if (exists("mangal1_summ")) {nnodes_MUT[[i]] <- mangal1_summ$n_nodes
  } else nnodes_MUT[[i]] <- nrow(mangal1$nodes)
  if (exists("mangal1_summ")) {nedges_MUT[[i]] <- mangal1_summ$n_edges
  } else nedges_MUT[[i]] <- nrow(mangal1$interactions)
  if (exists("mangal1_summ")) {connectance_MUT[[i]] <- mangal1_summ$connectance
  } else connectance_MUT[[i]] <- NA
  if (exists("mangal1_summ")) {linkage_density_MUT[[i]] <- mangal1_summ$linkage_density
  } else linkage_density_MUT[[i]] <- NA
  doi_MUT[[i]] <- mangal1$reference$doi
  year_MUT[[i]] <- mangal1$reference$year
  first_author_MUT[[i]] <- mangal1$reference$first_author
  paper_url_MUT[[i]] <- mangal1$reference$paper_url
  data_url_MUT[[i]] <- mangal1$reference$data_url
  
  message(paste0("Network ", i))
  
  rm(mangal1_summ)
  
}
#
for(i in 1:length(antagonistic_networks)){
  
  mangal1 <- antagonistic_networks[[i]]
  try({ mangal1_summ <- summary(mangal1)})
  
  if (exists("mangal1_summ")) {nnodes_ANT[[i]] <- mangal1_summ$n_nodes
  } else nnodes_ANT[[i]] <- nrow(mangal1$nodes)
  if (exists("mangal1_summ")) {nedges_ANT[[i]] <- mangal1_summ$n_edges
  } else nedges_ANT[[i]] <- nrow(mangal1$interactions)
  if (exists("mangal1_summ")) {connectance_ANT[[i]] <- mangal1_summ$connectance
  } else connectance_ANT[[i]] <- NA
  if (exists("mangal1_summ")) {linkage_density_ANT[[i]] <- mangal1_summ$linkage_density
  } else linkage_density_ANT[[i]] <- NA
  doi_ANT[[i]] <- mangal1$reference$doi
  year_ANT[[i]] <- mangal1$reference$year
  first_author_ANT[[i]] <- mangal1$reference$first_author
  paper_url_ANT[[i]] <- mangal1$reference$paper_url
  data_url_ANT[[i]] <- mangal1$reference$data_url
  
  message(paste0("Network ", i))
  
  rm(mangal1_summ)
  
}

nnodes_MUT <- as.numeric(nnodes_MUT)
nedges_MUT <- as.numeric(nedges_MUT)
connectance_MUT <- as.numeric(connectance_MUT)
linkage_density_MUT <- as.numeric(linkage_density_MUT)
doi_MUT <- as.character(doi_MUT)
year_MUT <- as.numeric(year_MUT)
first_author_MUT <- as.character(first_author_MUT)
paper_url_MUT <- as.character(paper_url_MUT)
data_url_MUT <- as.character(data_url_MUT)
#
nnodes_ANT <- as.numeric(nnodes_ANT)
nedges_ANT <- as.numeric(nedges_ANT)
connectance_ANT <- as.numeric(connectance_ANT)
linkage_density_ANT <- as.numeric(linkage_density_ANT)
doi_ANT <- as.character(doi_ANT)
year_ANT <- as.numeric(year_ANT)
first_author_ANT <- as.character(first_author_ANT)
paper_url_ANT <- as.character(paper_url_ANT)
data_url_ANT <- as.character(data_url_ANT)
#
nnodes <- c(nnodes_MUT, nnodes_ANT)
nedges <- c(nedges_MUT, nedges_ANT)
connectance <- c(connectance_MUT, connectance_ANT)
linkage_density <- c(linkage_density_MUT, linkage_density_ANT)
doi <- c(doi_MUT, doi_ANT)
year <- c(year_MUT, year_ANT)
first_author <- c(first_author_MUT, first_author_ANT)
paper_url <- c(paper_url_MUT, paper_url_ANT)
data_url <- c(data_url_MUT, data_url_ANT)

###Data frame with metrics and reference to which network ######################
metrics_and_references <- data.frame(
  nnodes,
  nedges,
  connectance,
  linkage_density,
  doi,
  year,
  first_author,
  paper_url,
  data_url
)

################################################################################
#                       CREATE SPATIAL POINT DATA FRAME
################################################################################

###Extracting spatial information ##############################################

xy_MUT <- list()
xy_ANT <- list()

for(i in 1:length(mutualistic_networks)){
  
  #if POINT get VALUE
  if(any(class(mutualistic_networks[[i]]$network$geom[[1]])=="POINT")){
    xy_MUT[[i]] <- mutualistic_networks[[i]]$network$geom[[1]]
  }
  
  #if polygon get centroid
  if(any(class(mutualistic_networks[[i]]$network$geom[[1]])=="POLYGON")){
    xy_MUT[[i]] <- st_centroid(mutualistic_networks[[i]]$network$geom[[1]])
  }
  
}
#
for(i in 1:length(antagonistic_networks)){
  
  #if POINT get VALUE
  if(any(class(antagonistic_networks[[i]]$network$geom[[1]])=="POINT")){
    xy_ANT[[i]] <- antagonistic_networks[[i]]$network$geom[[1]]
  }
  
  #if polygon get centroid
  if(any(class(antagonistic_networks[[i]]$network$geom[[1]])=="POLYGON")){
    xy_ANT[[i]] <- st_centroid(antagonistic_networks[[i]]$network$geom[[1]])
  }
  
}

world <- raster::shapefile("D:/sig/world_continents.shp")

for(i in 1:length(xy)){
  plot(xy[[i]], col="red", pch = 16, add=TRUE)
  message(paste0("Plot network ", i, "!"))
}

xy_2_MUT <- data.frame(matrix(ncol=2))
names(xy_2_MUT) <- c("x","y")
#
xy_2_ANT <- data.frame(matrix(ncol=2))
names(xy_2_ANT) <- c("x","y")

for(i in 1:length(xy_MUT)){
  xy_net <- xy_MUT[[i]]  
  xy_2_MUT[i,1] <- as.numeric(xy_net)[1]
  xy_2_MUT[i,2] <- as.numeric(xy_net)[2]
}
#
for(i in 1:length(xy_ANT)){
  xy_net <- xy_ANT[[i]]  
  xy_2_ANT[i,1] <- as.numeric(xy_net)[1]
  xy_2_ANT[i,2] <- as.numeric(xy_net)[2]
}

xy_2 <- rbind(xy_2_MUT, xy_2_ANT)
nrow(xy_2)

#Adding network number, dataset id and network description from mangal
network_number_MUT <- c()
network_description_MUT <- c()
dataset_id_MUT <- c()
#
network_number_ANT <- c()
network_description_ANT <- c()
dataset_id_ANT <- c()

for(i in 1:length(mutualistic_networks)){
  
  network_number_MUT[i] <- mutualistic_networks[[i]]$network$network_id
  network_description_MUT[i] <- mutualistic_networks[[i]]$network$description
  dataset_id_MUT[i] <- mutualistic_networks[[i]]$network$dataset_id
  
}
#
for(i in 1:length(antagonistic_networks)){
  
  network_number_ANT[i] <- antagonistic_networks[[i]]$network$network_id
  network_description_ANT[i] <- antagonistic_networks[[i]]$network$description
  dataset_id_ANT[i] <- antagonistic_networks[[i]]$network$dataset_id
  
}

network_number <- c(network_number_MUT, network_number_ANT)
network_description <- c(network_description_MUT, network_description_ANT)
dataset_id <- c(dataset_id_MUT, dataset_id_ANT)

#
network_number <- paste0("Network #", network_number)
dataset_id <- paste0("Dataset #", dataset_id)

#length(dataset_id)

#Create final (non-spatial) data frame #########################################
final_data_frame <- cbind(network_number, dataset_id, metrics_and_references[,c(1:4)], xy_2, fit_data_frame, network_description, metrics_and_references[,c(5:9)])

#Remove those without spatial info (to create spatial points data frame)
final_data_frame <- final_data_frame[!is.na(final_data_frame$x),]

#Create spatial point data frame
final_data_frame_SPATIAL <- SpatialPointsDataFrame(coords = final_data_frame[,7:8], 
                                                   data = final_data_frame, 
                                                   proj4string = world@proj4string)

#Plot
plot(world)

plot(final_data_frame_SPATIAL, pch=16, col=as.factor(final_data_frame_SPATIAL$type), add=TRUE)

################################################################################
#                          EXTRACT HUMAN DISTURBANCE
################################################################################

h_footprint <- raster::raster("D:/sig/wildareas-v3-2009-human-footprint-geotiff/wildareas-v3-2009-human-footprint.tif")
cumulative_oceans <- raster::raster("D:/sig/Human impact oceans 2013/global_cumul_impact_2013_all_layers.tif")
#
#Re-project
raster::crs(h_footprint)

h_footprint_P <- raster::projectRaster(from = h_footprint,
                                       crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
)

cumulative_oceans_P <- raster::projectRaster(
  from = cumulative_oceans,
  crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
)

plot(h_footprint_P)
plot(final_data_frame_SPATIAL, add=TRUE)
#
plot(cumulative_oceans_P)
plot(final_data_frame_SPATIAL, add=TRUE)

#Extracting the values
h_foot_vector <- raster::extract(h_footprint_P, final_data_frame_SPATIAL)
h_foot_vector[h_foot_vector==128] <- NA #128 are NA
c_ocean_vector <- raster::extract(cumulative_oceans_P, final_data_frame_SPATIAL)

################################################################################

#Is the network over a continent?    
over_continent <- sp::over(final_data_frame_SPATIAL, world)
over_continent <- over_continent$scalerank
over_continent[is.na(over_continent)] <- 0

################################################################################
#The final data frame
################################################################################

final_data_frame2 <- cbind(final_data_frame
                           ,h_foot_vector, 
                           c_ocean_vector, 
                           over_continent)

#save(final_data_frame2, file="final_29ABR_2022.RData")

################################################################################
## Add ecosystem

final_data_frame3 <- data.frame(final_data_frame2,NA)
names(final_data_frame3)[25] <- "ecosystem"

final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "ocean", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Ocean", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Seychelles, Indian Ocean", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Plant-pollinator", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "intertidal", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "shore", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "lake", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Lake", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Plant-polinator", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "flower", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "forest", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Pollination", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "fishery", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "river", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "birds", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "salt marsh", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "-pollinator", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "stream", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Current", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Gulf", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "oak", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Sea", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "tributaries", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "deep-sea", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "estuary", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "insect", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "marine", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "kelp", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "pine logs", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "Insect", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "anemonefish", negate = FALSE),]$ecosystem <- "marine"
final_data_frame3[stringr::str_detect(final_data_frame3$network_description, "fruit", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "southern California", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "pond", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "Bay", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "Laguna", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "Loch", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "River", negate = FALSE)) & is.na(final_data_frame3$ecosystem) ,]$ecosystem <- "freshwater"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "prairie", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "reef", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "bird", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "lagoon", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "freshwater", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "atoll", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "Aleutian", negate = FALSE)),]$ecosystem <- "marine"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "edge", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "Edge", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame3[(stringr::str_detect(final_data_frame3$network_description, "Malaysia", negate = FALSE)),]$ecosystem[2:3] <- "freshwater"

#Search one by one
#final_data_frame3[is.na(final_data_frame3$ecosystem),][1,]
#final_data_frame3[is.na(final_data_frame3$ecosystem),][1,]$ecosystem <- "terrestrial"

#Qtos faltam?
sum(is.na(final_data_frame3$ecosystem))

#Ver tabela
View(final_data_frame3)

save(final_data_frame3, file = "final_data_frame3.RData")

################################################################################
#                            Assortativity Index
################################################################################

#mutualistic_networks_igraph
#antagonistic_networks_igraph

assort_MUT <- c()
assort_ANT <- c()
#
#lapply(mutualistic_networks_igraph, is.directed)
#lapply(antagonistic_networks_igraph, is.directed)

for(i in 1:length(mutualistic_networks_igraph)){
  
  assort_MUT[i] <- assortativity_degree(mutualistic_networks_igraph[[i]], directed = igraph::is.directed(mutualistic_networks_igraph[[i]]))  
  
}
#
for(i in 1:length(antagonistic_networks_igraph)){
  
  assort_ANT[i] <- assortativity_degree(antagonistic_networks_igraph[[i]], directed = igraph::is.directed(antagonistic_networks_igraph[[i]]))  
  
}

#Get network ID
MUT_id <- c()
ANT_id <- c()

for(i in 1:length(mutualistic_networks)){
  MUT_id[i] <- mutualistic_networks[[i]]$network$network_id 
}
#
for(i in 1:length(antagonistic_networks)){
  ANT_id[i] <- antagonistic_networks[[i]]$network$network_id 
}

MUT_id <- paste0("Network #", MUT_id)
ANT_id <- paste0("Network #", ANT_id)

ASSORT_MUT <- data.frame(MUT_id, assort_MUT)
ASSORT_ANT <- data.frame(ANT_id, assort_ANT)

names(ASSORT_MUT) <- c("id", "assortativity_index")
names(ASSORT_ANT) <- c("id", "assortativity_index")

assortativity <- rbind(ASSORT_MUT, ASSORT_ANT)

final_data_frame4 <-merge (final_data_frame3, assortativity, by.x = 'network_number', by.y ='id')

View(final_data_frame4)
nrow(final_data_frame4)

#SAVE
save(final_data_frame4, file = "final_data_frame4.RData")

################################################################################
#                             Select directed graphs
################################################################################

is_directed <- c(unlist(lapply(mutualistic_networks_igraph, is.directed)),
                 unlist(lapply(antagonistic_networks_igraph, is.directed)))

net_id_is_directed_mut <- c()
net_id_is_directed_ant <- c()

for(i in 1:length(mutualistic_networks)){
  
  net_id_is_directed_mut[i] <- paste0("Network #", mutualistic_networks[[i]]$network$network_id)
  
}

for(i in 1:length(antagonistic_networks)){
  
  net_id_is_directed_ant[i] <- paste0("Network #", antagonistic_networks[[i]]$network$network_id)
  
}

net_id_is_directed <- c(net_id_is_directed_mut, net_id_is_directed_ant)

is_directed2 <- data.frame(net_id_is_directed, is_directed)
names(is_directed2)

final_data_frame5 <-merge(final_data_frame4, is_directed2, by.x = 'network_number', by.y ='net_id_is_directed')
View(final_data_frame5)

final_data_frame6 <- final_data_frame5[final_data_frame5$is_directed == TRUE,]
View(final_data_frame6)
nrow(final_data_frame6)
table(final_data_frame6$type)

#COrrect for the number of nodes and edges
names(final_data_frame6)[3] <- "nedges"
names(final_data_frame6)[4] <- "nnodes"

save(final_data_frame6, file = "final_data_frame6.RData")

names(final_data_frame6)
View(final_data_frame6)

################################################################################
#                                    HANPP
################################################################################
#12-05-2022
#load
hanpp_perc_npp <- raster("C:/fw_space/hapctnpp-geotiff/hapctnpp_geotiff.tif")

final_data_frame6_SPATIAL <- SpatialPointsDataFrame(coords = final_data_frame6[,7:8], data = final_data_frame6)

plot(hanpp_perc_npp)
plot(world, add=T)
plot(final_data_frame6_SPATIAL, add=T)

hanpp_vector <- extract(hanpp_perc_npp, final_data_frame6_SPATIAL)

final_data_frame7 <- data.frame(final_data_frame6, hanpp_vector)
View(final_data_frame7)

save(final_data_frame7, file="final_data_frame7.RData")

################################################################################

ant1 <- final_data_frame7[final_data_frame7$type=="antagonistic",]
mut1 <- final_data_frame7[final_data_frame7$type=="mutualistic",]

####

marine_df <- ant1[ant1$ecosystem == "marine",]
marine_df <- marine_df[!is.na(marine_df$c_ocean_vector),]

freshwater_df <- ant1[ant1$ecosystem == "freshwater",]
freshwater_df <- freshwater_df[!is.na(freshwater_df$h_foot_vector),]

ant2 <- ant1[!is.na(ant1$hanpp_vector),]
mut2 <- mut1[!is.na(mut1$hanpp_vector),]

ant3 <- ant2[ant2$hanpp_vector<12000,]

x= ant3$hanpp_vector
y= ant3$alpha_out
n=nrow(ant3)

plot(x,y)

#Models
# null
null=lm(y~1)
# exponential
lm(log(ant3$alpha_out+0.5) ~ log(ant3$hanpp_vector+0.5))
#
exponential=nls(y~a+b*log(x+0.5), start = list(a=1.95977, b=-0.02266))
#exponential=nlsLM(y~a+b*log(x+0.5), start = list(a=2.471, b=-1.769))

# assintotic
assintotic=nls(y~a+b/(x+0.5), start = list(a=1.95977, b=-0.02266)) 

AICctab(null,exponential,assintotic,nobs=n,weights = TRUE, delta = TRUE, base = TRUE)

#Plot data amd fitted models
plot(ant3$alpha_out ~ ant3$hanpp_vector , xlab="hanpp", ylab="Network parameter")
abline(null,col="green")

lines(coefficients(exponential)[1]+(coefficients(exponential)[2])*log(1:max(x)))
lines(coefficients(assintotic)[1]+(coefficients(assintotic)[2])/(1:max(x)), col="blue")

#3D

# Plot
plot3d( 
  x=ant1$hanpp_vector, y=ant1$alpha_out, z=mut1$nnodes,
  #col = mut1$connectance*100, 
  type = 's', 
  radius = 15*10,
  xlab="human impact", ylab="curve parameter", zlab="nodes")

names(mut1)

ant3 <- ant1[complete.cases(ant1),]
mod1 <-varpart(ant3$alpha_in, ant3$hanpp_vector, ant3$h_foot_vector)
plot(mod1)
summary(mod1)

###################
#FMestre
#26-05-2022
#Create spatial point data frame

final_data_frame_7_SPATIAL <- SpatialPointsDataFrame(coords = final_data_frame7[,7:8], 
                                                     data = final_data_frame7, 
                                                     proj4string = world@proj4string)

plot(world)
plot(final_data_frame_7_SPATIAL, add = TRUE)

################################################################################
#                               UPLOAD VARIABLES
################################################################################

#STRUDY SIDE
##COPERNICUS FRAGMENTATION
#f_2015 <- raster::raster("D:/cluster2/rasters/FGA2_S_2015_v01/FGA2_S_2015_v3.tif")
#f_2012 <- raster::raster("D:/cluster2/rasters/FGA2_S_2012_v01/FGA2_S_2012_v3.tif")
#f_2009 <- raster::raster("D:/cluster2/rasters/FGA2_S_2009_v01/FGA2_S_2009_v3.tif")

##HANPP and NPP
#NPP
npp_1990 <- raster("D:/cluster2/rasters/NPPpot_europe_1990_gCmyr.tif")
npp_2000 <- raster("D:/cluster2/rasters/NPPpot_europe_2000_gCmyr.tif")
npp_2006 <- raster("D:/cluster2/rasters/NPPpot_europe_2006_gCmyr.tif")
#HANPP
hanpp_1990 <- raster("D:/cluster2/rasters/hanpp_europe_1990_gCmyr.tif")
hanpp_2000 <- raster("D:/cluster2/rasters/hanpp_europe_2000_gCmyr.tif")
hanpp_2006 <- raster("D:/cluster2/rasters/hanpp_europe_2006_gCmyr.tif")
#As % of potential NPP
hanpp_perc_1990 <- raster("D:/cluster2/rasters/hanpp_europe_1990_percent.tif")
hanpp_perc_2000 <- raster("D:/cluster2/rasters/hanpp_europe_2000_percent.tif")
hanpp_perc_2006 <- raster("D:/cluster2/rasters/hanpp_europe_2006_percent.tif")

#WORLDCLIM
bio1_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio1.tif")
bio1_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio1.tif")
bio1_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio1.tif")
bio1 <- mean(bio1_80, bio1_90, bio1_00)
rm(bio1_80, bio1_90, bio1_00)
#
#
bio2_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio2.tif")
bio2_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio2.tif")
bio2_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio2.tif")
bio2 <- mean(bio2_80, bio2_90, bio2_00)
rm(bio2_80, bio2_90, bio2_00)
#
bio3_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio3.tif")
bio3_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio3.tif")
bio3_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio3.tif")
bio3 <- mean(bio3_80, bio3_90, bio3_00)
rm(bio3_80, bio3_90, bio3_00)
#
bio4_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio4.tif")
bio4_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio4.tif")
bio4_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio4.tif")
bio4 <- mean(bio4_80, bio4_90, bio4_00)
rm(bio4_80, bio4_90, bio4_00)
#
bio5_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio5.tif")
bio5_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio5.tif")
bio5_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio5.tif")
bio5 <- mean(bio5_80, bio5_90, bio5_00)
rm(bio5_80, bio5_90, bio5_00)
#
bio6_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio6.tif")
bio6_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio6.tif")
bio6_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio6.tif")
bio6 <- mean(bio6_80, bio6_90, bio6_00)
rm(bio6_80, bio6_90, bio6_00)
#
bio7_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio7.tif")
bio7_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio7.tif")
bio7_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio7.tif")
bio7 <- mean(bio7_80, bio7_90, bio7_00)
rm(bio7_80, bio7_90, bio7_00)
#
bio8_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio8.tif")
bio8_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio8.tif")
bio8_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio8.tif")
bio8 <- mean(bio8_80, bio8_90, bio8_00)
rm(bio8_80, bio8_90, bio8_00)
#
bio9_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio9.tif")
bio9_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio9.tif")
bio9_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio9.tif")
bio9 <- mean(bio9_80, bio9_90, bio9_00)
rm(bio9_80, bio9_90, bio9_00)
#
bio10_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio10.tif")
bio10_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio10.tif")
bio10_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio10.tif")
bio10 <- mean(bio10_80, bio10_90, bio10_00)
rm(bio10_80, bio10_90, bio10_00)
#
bio11_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio11.tif")
bio11_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio11.tif")
bio11_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio11.tif")
bio11 <- mean(bio11_80, bio11_90, bio11_00)
rm(bio11_80, bio11_90, bio11_00)
#
bio12_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio12.tif")
bio12_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio12.tif")
bio12_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio12.tif")
bio12 <- mean(bio12_80, bio12_90, bio12_00)
rm(bio12_80, bio12_90, bio12_00)
#
bio13_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio13.tif")
bio13_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio13.tif")
bio13_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio13.tif")
bio13 <- mean(bio13_80, bio13_90, bio13_00)
rm(bio13_80, bio13_90, bio13_00)
#
bio14_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio14.tif")
bio14_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio14.tif")
bio14_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio14.tif")
bio14 <- mean(bio14_80, bio14_90, bio14_00)
rm(bio14_80, bio14_90, bio14_00)
#
bio15_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio15.tif")
bio15_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio15.tif")
bio15_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio15.tif")
bio15 <- mean(bio15_80, bio15_90, bio15_00)
rm(bio15_80, bio15_90, bio15_00)
#
bio16_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio16.tif")
bio16_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio16.tif")
bio16_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio16.tif")
bio16 <- mean(bio16_80, bio16_90, bio16_00)
rm(bio16_80, bio16_90, bio16_00)
#
bio17_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio17.tif")
bio17_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio17.tif")
bio17_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio17.tif")
bio17 <- mean(bio17_80, bio17_90, bio17_00)
rm(bio17_80, bio17_90, bio17_00)
#
bio18_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio18.tif")
bio18_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio18.tif")
bio18_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio18.tif")
bio18 <- mean(bio18_80, bio18_90, bio18_00)
rm(bio18_80, bio18_90, bio18_00)
#
bio19_80 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_80s_bio19.tif")
bio19_90 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_90s_bio19.tif")
bio19_00 <- raster("D:/Dados climáticos/MERRAclim/10m_mean_00s_bio19.tif")
bio19 <- mean(bio19_80, bio19_90, bio19_00)
rm(bio19_80, bio19_90, bio19_00)
#

#WORLDCLIM
max_temp1 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_01.tif")
max_temp2 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_02.tif")
max_temp3 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_03.tif")
max_temp4 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_04.tif")
max_temp5 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_05.tif")
max_temp6 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_06.tif")
max_temp7 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_07.tif")
max_temp8 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_08.tif")
max_temp9 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_09.tif")
max_temp10 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_10.tif")
max_temp11 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_11.tif")
max_temp12 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmax/wc2.1_10m_tmax_12.tif")
#
min_temp1 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_01.tif")
min_temp2 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_02.tif")
min_temp3 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_03.tif")
min_temp4 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_04.tif")
min_temp5 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_05.tif")
min_temp6 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_06.tif")
min_temp7 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_07.tif")
min_temp8 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_08.tif")
min_temp9 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_09.tif")
min_temp10 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_10.tif")
min_temp11 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_11.tif")
min_temp12 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_tmin/wc2.1_10m_tmin_12.tif")
#
av_temp1 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_01.tif")
av_temp2 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_02.tif")
av_temp3 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_03.tif")
av_temp4 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_04.tif")
av_temp5 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_05.tif")
av_temp6 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_06.tif")
av_temp7 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_07.tif")
av_temp8 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_08.tif")
av_temp9 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_09.tif")
av_temp10 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_10.tif")
av_temp11 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_11.tif")
av_temp12 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_tavg/wc2.0_10m_tavg_12.tif")
#
prec1 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_01.tif")
prec2 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_02.tif")
prec3 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_03.tif")
prec4 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_04.tif")
prec5 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_05.tif")
prec6 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_06.tif")
prec7 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_07.tif")
prec8 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_08.tif")
prec9 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_09.tif")
prec10 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_10.tif")
prec11 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_11.tif")
prec12 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.0_10m_prec/wc2.0_10m_prec_12.tif")
#
rad1 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_01.tif")
rad2 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_02.tif")
rad3 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_03.tif")
rad4 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_04.tif")
rad5 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_05.tif")
rad6 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_06.tif")
rad7 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_07.tif")
rad8 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_08.tif")
rad9 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_09.tif")
rad10 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_10.tif")
rad11 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_11.tif")
rad12 <- raster("D:/Dados climáticos/WorldClim 2.0/wc2.1_10m_srad/wc2.1_10m_srad_12.tif")

max_temp <- mean(max_temp1, max_temp2, max_temp3, max_temp4, max_temp5, 
                 max_temp6, max_temp7, max_temp8, max_temp9, max_temp10, max_temp11, max_temp12)
min_temp <- mean(min_temp1, min_temp2, min_temp3, min_temp4, min_temp5, min_temp6, 
                 min_temp7, min_temp8, min_temp9, min_temp10, min_temp11, min_temp12)
average_temp <- mean(av_temp1, av_temp2, av_temp3, av_temp4, av_temp5, av_temp6, 
                     av_temp7, av_temp8, av_temp9, av_temp10, av_temp11, av_temp12)
precipitation <- mean(prec1, prec2, prec3, prec4, prec5, prec6, prec7, prec8, 
                      prec9, prec10, prec11, prec12)
solar_radiation <- mean(rad1, rad2, rad3, rad4, rad5, rad6, rad7, rad8, rad9,
                        rad10, rad11, rad12)
#

#Bio-Oracle
mean_depth_primary_prod <- raster("D:/Dados climáticos/Bio-ORACLE/Present.Benthic.Mean.Depth.Primary.productivity.Mean.BOv2_2.tif/Present.Benthic.Mean.Depth.Primary.productivity.Mean.tif")
surface_primary_prod <- raster("D:/Dados climáticos/Bio-ORACLE/Present.Surface.Primary.productivity.Mean.BOv2_2.tif/Present.Surface.Primary.productivity.Mean.tif")

#CREATE STACKS
#copernicus_frag <- stack(f_2009, f_2012, f_2015)
npp <- stack(npp_1990, npp_2000, npp_2006)
hanpp <- stack(hanpp_1990, hanpp_2000, hanpp_2006)
hanpp_perc <- stack(hanpp_perc_1990, hanpp_perc_2000, hanpp_perc_2006)
merraclim_bioclimatic <- stack(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, 
                               bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17,
                               bio18, bio19)
worldclim_others <- stack(max_temp, min_temp, average_temp, precipitation, solar_radiation)
bio_oracle <- stack(mean_depth_primary_prod, surface_primary_prod)

#EXTRACT
#copernicus_frag_extracted <- raster::extract(copernicus_frag, final_data_frame_7_SPATIAL)
npp_extracted <- raster::extract(npp, final_data_frame_7_SPATIAL)
hanpp_extracted <- raster::extract(hanpp, final_data_frame_7_SPATIAL)
hanpp_perc_extracted <- raster::extract(hanpp_perc, final_data_frame_7_SPATIAL)
#
merraclim_bioclimatic_extracted <- raster::extract(merraclim_bioclimatic, final_data_frame_7_SPATIAL)
worldclim_others_extracted <- raster::extract(worldclim_others, final_data_frame_7_SPATIAL)
#
bio_oracle_extracted <- raster::extract(bio_oracle, final_data_frame_7_SPATIAL)
#
vars1 <- data.frame(
  #copernicus_frag_extracted,
  npp_extracted,
  hanpp_extracted,
  hanpp_perc_extracted,
  merraclim_bioclimatic_extracted,
  worldclim_others_extracted,
  bio_oracle_extracted
)

names(vars1) <- c(#"copernicus_frag_2009", "copernicus_frag_2012", "copernicus_frag_2015", 
  "npp_1990", "npp_2000", "npp_2006", 
  "hanpp_1990", "hanpp_2000", "hanpp_2006", 
  "hanpp_perc_1990", "hanpp_perc_2000", "hanpp_perc_2006", 
  "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
  "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", 
  "bio17", "bio18", "bio19", 
  "max_temp", "min_temp", "average_temp", "precipitation", "solar_radiation",
  "marine_mean_depth_PP", "marine_surface_PP")

View(vars1)

#plot(solar_radiation)
#plot(final_data_frame_7_SPATIAL, add=TRUE)


final_data_frame8 <- data.frame(final_data_frame7, vars1)
View(final_data_frame8)

save(final_data_frame8, file = "final_data_frame8.RData")

#final_data_frame8[(stringr::str_detect(final_data_frame8$network_description, "xxx", negate = FALSE)),]

plot(final_data_frame8$alpha_in,
     final_data_frame8$h_foot_vector)


final_data_frame9 <- final_data_frame8[final_data_frame8$p_in>0.05 & final_data_frame8$p_out>0.05,]


library(rgl)
plot3d(x=final_data_frame9$alpha_in,
       y=final_data_frame9$h_foot_vector, 
       z=final_data_frame9$alpha_out, 
       type = "s", size = 0.75, lit = FALSE)


##
#22-07-2022
#FMestre

View(final_data_frame9)
#save(final_data_frame9, file = "final_data_frame9.RData")

#FMestre
#final_data_frame9
#22-09-2022

cor(final_data_frame9$solar_radiation,final_data_frame9$bio1, use="complete.obs")

table(final_data_frame9$type)
table(final_data_frame9$ecosystem)


names(final_data_frame9)

final_data_frame9$over_continent

final_data_frame9[final_data_frame9$type=="mutualistic",]


final_data_frame9_SPATIAL <- sp::SpatialPointsDataFrame(coords = final_data_frame9[,7:8], data = final_data_frame9)

rgdal::writeOGR(obj=final_data_frame9_SPATIAL, dsn="final_data_frame9_SPATIAL", layer="final_data_frame9_SPATIAL", driver="ESRI Shapefile")

################################################################################
#         REMOVE SMALLER FOOD WEBS? THOSE WITH SMALLER MAX DEGREE?
################################################################################

nnodes_OF_all_mangal_objects_selected_igraph <- c()

for(i in 1:length(all_mangal_objects_selected_igraph)){
  
  nnodes_OF_all_mangal_objects_selected_igraph[i] <- igraph::gorder(all_mangal_objects_selected_igraph[[i]])
  
}

nnodes_OF_all_mangal_objects_selected_igraph

###

max_degree_OF_all_mangal_objects_selected_igraph <- c()

for(i in 1:length(all_mangal_objects_selected_igraph)){
  
  max_degree_OF_all_mangal_objects_selected_igraph[i] <- max(igraph::degree(all_mangal_objects_selected_igraph[[i]]))
  
}

max_degree_OF_all_mangal_objects_selected_igraph

################################################################################
#
################################################################################

#data I have currently
all_mangal_objects_selected_igraph
all_mangal_objects_selected

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

beanplot(in_overall_vector ~ fact1)

#out - overall
fact1 <- c(rep("location", 310), rep("size", 310), rep("shape", 310))
out_overall_vector <- c(sq_wasserstein_out_overall_location_PERC, sq_wasserstein_out_overall_size_PERC, sq_wasserstein_out_overall_shape_PERC)
df_out_overall_vect <- data.frame(fact1, out_overall_vector)

boxplot(out_overall_vector ~ fact1)

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


#generate a html of lme results
tab_model(mod.lme, digits=2,file = "assortativity_index_mutualism.html")

#plot
result=model_parameters(mod.lme_MUT)
jpeg(file="assortativity_index_mutualism.jpeg")
plot(result)

#Save to shapefile
names(final_data_frame_13)
final_data_frame_13_SPDF <- SpatialPointsDataFrame(coords = final_data_frame_13[,7:8], data = final_data_frame_13)
rgdal::writeOGR(obj=final_data_frame_13_SPDF, dsn="tempdir", layer="final_data_frame_13_SPDF", driver="ESRI Shapefile")

#nrow(final_data_frame_13_SPDF)
#table(final_data_frame_13_SPDF$type)

################################################################################
#                                   TREES
################################################################################

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
#                               Deriving CRRnt
################################################################################
#FMestre
#04-11-2022

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
rpart.plot::rpart.plot(FW_tree)
rpart.plot::rpart.plot(FW_tree_pruned)
rpart.plot::rpart.plot(MUT_tree)
rpart.plot::rpart.plot(MUT_tree_pruned)

################################################################################
# Creating trees with the overall distance only
################################################################################
#FMestre
#14-11-2022

sq_wasserstein_in_out2 <- data.frame(code_network,
                                     sq_wasserstein_in_out_distance, 
                                     sq_wasserstein_in_out_location_PERC,
                                     sq_wasserstein_in_out_size_PERC,
                                     sq_wasserstein_in_out_shape_PERC
)

retrieve_codes <- stringr::str_split(sq_wasserstein_in_out2$code_network, "_")

retrieve_codes2 <- c()

for(i in 1:length(retrieve_codes))  retrieve_codes2[i] <- retrieve_codes[[i]][2]
 
sq_wasserstein_in_out2 <- data.frame(retrieve_codes2,
                                     sq_wasserstein_in_out2
)

sq_wasserstein_in_out2 <- data.frame(sq_wasserstein_in_out2$retrieve_codes2,
                                     sq_wasserstein_in_out2$sq_wasserstein_in_out_distance)

names(sq_wasserstein_in_out2) <- c("codes", "distance")

final_data_frame_15 <- merge(x=final_data_frame_14, y=sq_wasserstein_in_out2, by.x = "network_number", by.y = "codes")
nrow(final_data_frame_15)
View(final_data_frame_15)


#Separate it per type of network 
final_data_frame_15_FW <- final_data_frame_15[final_data_frame_15$type=="antagonistic",]
final_data_frame_15_MUT <- final_data_frame_15[final_data_frame_15$type=="mutualistic",]


names(final_data_frame_15_FW)

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

##Recovering multivariate trees with 3 distances

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

#3 distances
#rpart.plot(MUT_tree, box.palette="orange4", shadow.col="gray", nn=TRUE)
#rpart.plot(FW_tree, box.palette="yellow4", shadow.col="gray", nn=TRUE)

#overall distance
rpart.plot(rpart_MUT_3, box.palette="orange3", shadow.col="gray", nn=TRUE)
rpart.plot(rpart_FW_3, box.palette="yellow3", shadow.col="gray", nn=TRUE)

###
#21-11-2022

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

################################################################################################################################################################
################################################################################################################################################################



