#FMestre
#16-03-2022

#Using rmangal

library(rmangal)
library(igraph)
library(sp)

#"mutualism", "predation", "herbivory", "scavenger", "detritivore"

mangal_mutualism <- search_interactions(type = "mutualism", verbose = TRUE)
mangal_predation <- search_interactions(type = "predation", verbose = TRUE)
mangal_herbivory <- search_interactions(type = "herbivory", verbose = TRUE)
#mangal_scavenger <- search_interactions(type = "scavenger", verbose = TRUE)
#mangal_detritivore <- search_interactions(type = "detritivore", verbose = TRUE)

View(mangal_all)

#Select directed
#mangal_mutualism_dir <-  mangal_mutualism[mangal_mutualism$direction == "directed",]
#mangal_predation_dir <- mangal_predation[mangal_predation$direction == "directed",]
#mangal_herbivory_dir <- mangal_herbivory[mangal_herbivory$direction == "directed",]
#mangal_scavenger_dir <- mangal_scavenger[mangal_scavenger$direction == "directed",]
#mangal_detritivore_dir <- mangal_detritivore[mangal_detritivore$direction == "directed",]

#mangal_all <- rbind(mangal_mutualism_dir,
#                    mangal_predation_dir,
#                    mangal_herbivory_dir,
#                    mangal_scavenger_dir,
#                    mangal_detritivore_dir)

mutualistic_networks <- get_collection(mangal_mutualism, as_sf = TRUE, verbose = TRUE)
#mutualistic_networks <- unique (mutualistic_networks)
#
mangal_collection_predation <- get_collection(mangal_predation, as_sf = TRUE, verbose = TRUE)
mangal_collection_herbivory <- get_collection(mangal_herbivory, as_sf = TRUE, verbose = TRUE)
antagonistic_networks <- combine_mgNetworks(mangal_collection_predation, mangal_collection_herbivory)
antagonistic_networks <- unique(antagonistic_networks)

#mangal_collection_scavenger_dir <- get_collection(mangal_scavenger_dir, as_sf = TRUE, verbose = TRUE)
#mangal_collection_detritivore_dir <- get_collection(mangal_detritivore_dir, as_sf = TRUE, verbose = TRUE)

#mangal_collection <- combine_mgNetworks(mangal_collection_mutualism_dir, mangal_collection_predation_dir, 
#                                        mangal_collection_herbivory_dir, mangal_collection_scavenger_dir, 
#                                        mangal_collection_detritivore_dir)


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


#Which don't have only degrees 0 and 1
#non_excluded_MUT <- as.numeric(lapply(in_degree_list, sum)) > as.numeric(lapply(in_degree_list, length))
#mangal_collection_igraph_2 <- mangal_collection_igraph[non_excluded]
#table(non_excluded)
#length(mangal_collection_igraph_2)
#save(mangal_collection_igraph_2, file="mangal_collection_igraph_2.RData")

##

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

#View(fit_data_frame)
#nrow(fit_data_frame)

#Using mangal to get other information
#length(mangal_collection)
#length(non_excluded)
#mangal_collection_2 <- mangal_collection[non_excluded]
#length(mangal_collection_2)

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

#View(metrics_and_references)
#nrow(metrics_and_references)

################################################################################
#CREATE SPATIAL POINT DATA FRAME
################################################################################

###Extracting spatial information ##############################################

#xy <- data.frame(matrix(ncol = 2))
#names(xy) <- c("x","y")

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

#lapply(xy,class)

#world <- raster::shapefile("C:/Users/FMest/Documents/shape/ne_110m_admin_0_countries.shp")
world <- raster::shapefile("D:/sig/world_continents.shp")

for(i in 1:length(xy)){
  plot(xy[[i]], col="red", pch = 16, add=TRUE)
  message(paste0("Plot network ", i, "!"))
}

#length(xy)

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
#View(final_data_frame)
#nrow(final_data_frame)

#Remove those without spatial info (to create spatial points data frame)
final_data_frame <- final_data_frame[!is.na(final_data_frame$x),]
#nrow(final_data_frame)
#View(final_data_frame)
#names(final_data_frame)

#Select the same networks from the mangal collection
#mangal_collection_3 <- mangal_collection_2[!is.na(xy_2$x)]

#Create spatial point data frame
final_data_frame_SPATIAL <- SpatialPointsDataFrame(coords = final_data_frame[,7:8], 
                       data = final_data_frame, 
                       proj4string = world@proj4string)

#nrow(final_data_frame_SPATIAL)

#Plot
plot(world)

plot(final_data_frame_SPATIAL, pch=16, col=as.factor(final_data_frame_SPATIAL$type), add=TRUE)

################################################################################
# EXTRACT HUMAN DISTURBANCE
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
#Assortativity Index
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
#Select directed graphs
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
#HANPP##########################################################################
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

################################################################################


ant1 <- final_data_frame7[final_data_frame7$type=="antagonistic",]
mut1 <- final_data_frame7[final_data_frame7$type=="mutualistic",]


####

library(bbmle)
library(minpack.lm)

marine_df <- ant1[ant1$ecosystem == "marine",]
marine_df <- marine_df[!is.na(marine_df$c_ocean_vector),]

freshwater_df <- ant1[ant1$ecosystem == "freshwater",]
freshwater_df <- freshwater_df[!is.na(freshwater_df$h_foot_vector),]

ant2 <- ant1[!is.na(ant1$hanpp_vector),]
mut2 <- mut1[!is.na(mut1$hanpp_vector),]

x= ant2$hanpp_vector
y= ant2$alpha_out
n=nrow(ant2)

plot(x,y)

#Models
# null
null=lm(y~1)
# exponential
lm(log(ant2$hanpp_vector+0.5) ~ log(ant2$alpha_out+0.5))
#
exponential=nls(y~a+b*log(x+0.5), start = list(a=2.3805, b=0.6383))
#exponential=nlsLM(y~a+b*log(x+0.5), start = list(a=2.471, b=-1.769))

# assintotic
assintotic=nls(y~a+b/(x+0.5), start = list(a=2.3805, b=0.6383)) 

AICctab(null,exponential,assintotic,nobs=n,weights = TRUE, delta = TRUE, base = TRUE)

#Plot data amd fitted models
plot(ant2$hanpp_vector ~ ant2$alpha_out, ylab="hanpp", xlab="Network parameter")
abline(null,col="green")

lines(coefficients(exponential)[1]+(coefficients(exponential)[2])*log(1:max(x)))
lines(coefficients(assintotic)[1]+(coefficients(assintotic)[2])/(1:max(x)), col="blue")


#3D

# library
library(rgl)

# Plot
plot3d( 
  x=mut1$h_foot_vector, y=mut1$alpha_out, z=mut1$nnodes, 
  #col = mut1$connectance*100, 
  type = 's', 
  radius = 15,
  xlab="human footprint", ylab="curve parameter", zlab="nodes")


