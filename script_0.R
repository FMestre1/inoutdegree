#FMestre
#16-03-2022

#Using rmangal

library(rmangal)
library(igraph)

#"mutualism", "predation", "herbivory", "scavenger", "detritivore"

mangal_mutualism <- search_interactions(type = "mutualism", verbose = TRUE)
mangal_predation <- search_interactions(type = "predation", verbose = TRUE)
mangal_herbivory <- search_interactions(type = "herbivory", verbose = TRUE)
mangal_scavenger <- search_interactions(type = "scavenger", verbose = TRUE)
mangal_detritivore <- search_interactions(type = "detritivore", verbose = TRUE)

#Select directed
mangal_mutualism_dir <-  mangal_mutualism[mangal_mutualism$direction == "directed",]
mangal_predation_dir <- mangal_predation[mangal_predation$direction == "directed",]
mangal_herbivory_dir <- mangal_herbivory[mangal_herbivory$direction == "directed",]
mangal_scavenger_dir <- mangal_scavenger[mangal_scavenger$direction == "directed",]
mangal_detritivore_dir <- mangal_detritivore[mangal_detritivore$direction == "directed",]

mangal_collection_mutualism_dir <- get_collection(mangal_mutualism_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_predation_dir <- get_collection(mangal_predation_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_herbivory_dir <- get_collection(mangal_herbivory_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_scavenger_dir <- get_collection(mangal_scavenger_dir, as_sf = TRUE, verbose = TRUE)
mangal_collection_detritivore_dir <- get_collection(mangal_detritivore_dir, as_sf = TRUE, verbose = TRUE)

mangal_collection <- combine_mgNetworks(mangal_collection_mutualism_dir, mangal_collection_predation_dir, 
                                        mangal_collection_herbivory_dir, mangal_collection_scavenger_dir, 
                                        mangal_collection_detritivore_dir)

#Save
length(mangal_collection)
save(mangal_collection, file="mangal_dataset_mangal.RData")

#Convert to igraph
mangal_collection_igraph <- as.igraph(mangal_collection)
length(mangal_collection_igraph)
save(mangal_collection_igraph, file="mangal_dataset_igraph.RData")

#Getting the "in" and "out" degree of all networks
in_degree_list <- list()
out_degree_list <- list()

for(i in 1:length(mangal_collection_igraph)){

in_degree_list[[i]] <- degree(mangal_collection_igraph[[i]], mode="in")
out_degree_list[[i]] <- degree(mangal_collection_igraph[[i]], mode="out")
  
message(paste0("Concluded matrix ", i, "!"))

}

#Which dont have only degrees 0 and 1
non_excluded <- as.numeric(lapply(in_degree_list, sum)) > as.numeric(lapply(in_degree_list, length))
mangal_collection_igraph_2 <- mangal_collection_igraph[non_excluded]
table(non_excluded)
length(mangal_collection_igraph_2)

##

in_degree_list_2 <- list()
out_degree_list_2 <- list()

for(i in 1:length(mangal_collection_igraph_2)){
  
  in_degree_list_2[[i]] <- as.numeric(degree_distribution(mangal_collection_igraph_2[[i]], cumulative = TRUE, mode = "in"))
  out_degree_list_2[[i]] <- as.numeric(degree_distribution(mangal_collection_igraph_2[[i]], cumulative = TRUE, mode = "out"))
  
  message(paste0("Concluded matrix ", i, "!"))
  
}

alpha_in <- c()
alpha_out <- c()
p_in <- c()
p_out <- c()
log_lik_in <- c()
log_lik_out <- c()

for(i in 1:length(mangal_collection_igraph_2)){
  
  fit_in <- fit_power_law(in_degree_list_2[[i]]+1)
  fit_out <- fit_power_law(out_degree_list_2[[i]]+1)
  
  
  alpha_in[i] <- fit_in$alpha
  alpha_out[i] <- fit_out$alpha
  p_in[i] <- fit_in$KS.p
  p_out[i] <- fit_out$KS.p
  log_lik_in[i] <- fit_in$logLik
  log_lik_out[i] <- fit_out$logLik
  
  message(paste0("Network ", i))
  
}

###Data frame with in and out degree distributiin fit ##########################

fit_data_frame <- data.frame(
  alpha_in,
  alpha_out,
  p_in,
  p_out,
  log_lik_in,
  log_lik_out
)

View(fit_data_frame)
nrow(fit_data_frame)


#Using mangal to get other information
length(mangal_collection)
length(non_excluded)
mangal_collection_2 <- mangal_collection[non_excluded]
length(mangal_collection_2)

nnodes <- c()
nedges <- c()
connectance <- c()
linkage_density <- c()
doi <- c()
year <- c()
first_author <- c()
paper_url <- c()
data_url <- c()

for(i in 1:length(mangal_collection_2)){
  
  mangal1 <- mangal_collection_2[[i]]
  try({ mangal1_summ <- summary(mangal1)})
  
  if (exists("mangal1_summ")) {nnodes[[i]] <- mangal1_summ$n_nodes
                              } else nnodes[[i]] <- nrow(mangal1$nodes)
  if (exists("mangal1_summ")) {nedges[[i]] <- mangal1_summ$n_edges
                              } else nedges[[i]] <- nrow(mangal1$interactions)
  if (exists("mangal1_summ")) {connectance[[i]] <- mangal1_summ$connectance
                              } else connectance[[i]] <- NA
  if (exists("mangal1_summ")) {linkage_density[[i]] <- mangal1_summ$linkage_density
                              } else linkage_density[[i]] <- NA
  doi[[i]] <- mangal1$reference$doi
  year[[i]] <- mangal1$reference$year
  first_author[[i]] <- mangal1$reference$first_author
  paper_url[[i]] <- mangal1$reference$paper_url
  data_url[[i]] <- mangal1$reference$data_url
  
  message(paste0("Network ", i))
  
  rm(mangal1_summ)
  
}

nnodes <- as.numeric(nnodes)
nedges <- as.numeric(nedges)
connectance <- as.numeric(connectance)
linkage_density <- as.numeric(linkage_density)
doi <- as.character(doi)
year <- as.numeric(year)
first_author <- as.character(first_author)
paper_url <- as.character(paper_url)
data_url <- as.character(data_url)


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

View(metrics_and_references)
nrow(metrics_and_references)


################################################################################
#CREATE SPATIA POINT DATA FRAME
################################################################################


###Extracting spatial information ##############################################

#xy <- data.frame(matrix(ncol = 2))
#names(xy) <- c("x","y")

xy <- list()

for(i in 1:length(mangal_collection_2)){
  
  #if POINT get VALUE
  if(any(class(mangal_collection_2[[i]]$network$geom[[1]])=="POINT")){
    xy[[i]] <- mangal_collection_2[[i]]$network$geom[[1]]
  }
  
  #if polygon get centroid
  if(any(class(mangal_collection_2[[i]]$network$geom[[1]])=="POLYGON")){
    xy[[i]] <- st_centroid(mangal_collection_2[[i]]$network$geom[[1]])
  }
  
}

#lapply(xy,class)

#world <- raster::shapefile("C:/Users/FMest/Documents/shape/ne_110m_admin_0_countries.shp")
world <- raster::shapefile("D:/sig/world_continents.shp")

library(sp)
#
plot(world)

for(i in 1:length(xy)){
  plot(xy[[i]], col="red", pch = 16, add=TRUE)
  message(paste0("Plot network ", i, "!"))
}

#length(xy)

xy_2 <- data.frame(matrix(ncol=2))
names(xy_2) <- c("x","y")

for(i in 1:length(xy)){
  xy_net <- xy[[i]]  
  xy_2[i,1] <- as.numeric(xy_net)[1]
  xy_2[i,2] <- as.numeric(xy_net)[2]
}


#Create final (non-spatial) data frame #########################################
final_data_frame <- cbind(metrics_and_references, xy_2, fit_data_frame)
View(final_data_frame)


#Remove those without spatial info (to create spatial points data frame)
final_data_frame <- final_data_frame[!is.na(final_data_frame$x),]
#nrow(final_data_frame)
#View(final_data_frame)
#names(final_data_frame)

#Create spatial point data frame
final_data_frame_SPATIAL <- SpatialPointsDataFrame(coords = final_data_frame[,10:11], 
                       data = final_data_frame, 
                       proj4string = CRS(as.character(NA)))

#Plot
plot(world)
plot(final_data_frame_SPATIAL, pch=16, col="red", add=TRUE)

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
final_data_frame_SPATIAL@proj4string <- world@proj4string # I know the spatialpointsdataframe is in WGS84
over_continent <- sp::over(final_data_frame_SPATIAL, world)
over_continent <- over_continent$scalerank
over_continent[is.na(over_continent)] <- 0

################################################################################
#The final data frame
################################################################################

final_data_frame <- cbind(final_data_frame, over_continent, h_foot_vector, c_ocean_vector)
#View(final_data_frame)

final_30MAR_2022 <- final_data_frame
View(final_30MAR_2022)

save(final_30MAR_2022, file="final_30MAR_2022.RData")

#plot(final_data_frame$alpha_in, final_data_frame$h_foot_vector)
#plot(final_data_frame$alpha_out, final_data_frame$h_foot_vector)
#
#plot(final_data_frame$alpha_in, final_data_frame$c_ocean_vector)
#plot(final_data_frame$alpha_out, final_data_frame$c_ocean_vector)

