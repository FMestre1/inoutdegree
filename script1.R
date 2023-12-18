################################################################################
#                                                                              #
#      Towards interpreting in- and out-degree in ecological networks          #
#    Mestre, F.; Bastazini, V.A.G.; Galiana, N.; Rozenfeld, A.; Araújo, M.B.   #
#                                                                              #
################################################################################

#FMestre
#04-12-2023

#Load packages
library(waddR)
library(rpart)
library(rmangal)
library(terra)
library(igraph)
library(sf)
library(sp)
library(waddR)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(viridis)
library(ggpubr)

################################################################################
# 1. Network data importing
################################################################################

mangal_mutualism <- search_interactions(type = "mutualism", verbose = TRUE)
mangal_predation <- search_interactions(type = "predation", verbose = TRUE)
mangal_herbivory <- search_interactions(type = "herbivory", verbose = TRUE)

mangal_mutualism <- unique(mangal_mutualism)
mangal_predation <- unique(mangal_predation)
mangal_herbivory <- unique(mangal_herbivory)

mutualistic_networks <- get_collection(mangal_mutualism, as_sf = TRUE, verbose = TRUE)

mangal_collection_predation <- get_collection(mangal_predation, as_sf = TRUE, verbose = TRUE)
mangal_collection_herbivory <- get_collection(mangal_herbivory, as_sf = TRUE, verbose = TRUE)
antagonistic_networks <- combine_mgNetworks(mangal_collection_predation, mangal_collection_herbivory)
antagonistic_networks <- unique(antagonistic_networks)

#Save
#save(mutualistic_networks, file="mutualistic_networks.RData")
#save(antagonistic_networks, file="antagonistic_networks.RData")

class(antagonistic_networks) <- "mgNetworksCollection"

all_mangal_objects <- combine_mgNetworks(mutualistic_networks, antagonistic_networks)

#Remove un-used objects
rm(mangal_mutualism,
   mangal_predation,
   mangal_herbivory,
   mangal_collection_predation,
   mangal_collection_herbivory)

#Convert to igraph
mutualistic_networks_igraph <- as.igraph(mutualistic_networks)
antagonistic_networks_igraph <- as.igraph(antagonistic_networks)

################################################################################
# 2. In- and out-degree
################################################################################

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
#
network_number_list_MUT <- c()
network_number_list_ANT <- c()
#
for(i in 1:length(mutualistic_networks)) network_number_list_MUT[i] <- paste0("Network #", mutualistic_networks[[i]]$network$network_id)
for(i in 1:length(antagonistic_networks)) network_number_list_ANT[i] <- paste0("Network #", antagonistic_networks[[i]]$network$network_id)
  
network_number_list <- c(network_number_list_MUT, network_number_list_ANT)
  
in_degree_list <- c(in_degree_list_2_MUT, in_degree_list_2_ANT)
out_degree_list <- c(out_degree_list_2_MUT, out_degree_list_2_ANT)

names(in_degree_list) <- network_number_list
names(out_degree_list) <- network_number_list

################################################################################
# Create Geographic Table
################################################################################

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

#Convert to dataframe
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
#nrow(xy_2)

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

#Metrics and references
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

#Combine
type_MUT <- rep("mutualistic",length(mutualistic_networks_igraph))
type_ANT <- rep("antagonistic",length(antagonistic_networks_igraph))
type <- c(type_MUT, type_ANT)

###Data frame with metrics and reference to which network
metrics_and_references <- data.frame(
  type,
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

rm(
  type,
  nnodes,
  nedges,
  connectance,
  linkage_density,
  doi,
  year,
  first_author,
  paper_url,
  data_url,
  type_ANT,
  nnodes_ANT,
  nedges_ANT,
  connectance_ANT,
  linkage_density_ANT,
  doi_ANT,
  year_ANT,
  first_author_ANT,
  paper_url_ANT,
  data_url_ANT,
  type_MUT,
  nnodes_MUT,
  nedges_MUT,
  connectance_MUT,
  linkage_density_MUT,
  doi_MUT,
  year_MUT,
  first_author_MUT,
  paper_url_MUT,
  data_url_MUT
)

#View(metrics_and_references)

#Create final (non-spatial) data frame #########################################
final_data_frame <- cbind(network_number, dataset_id, metrics_and_references[,c(1:5)], xy_2, network_description, metrics_and_references[,c(6:9)])

#Remove networks without coordinates
final_data_frame <- final_data_frame[!is.na(final_data_frame$x),]
#names(final_data_frame)
#View(final_data_frame)

################################################################################
# Add Ecosystem Information
################################################################################

final_data_frame_2 <- cbind(final_data_frame,NA)
names(final_data_frame_2)[15] <- "ecosystem"

final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "ocean", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Ocean", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Seychelles, Indian Ocean", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Plant-pollinator", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "intertidal", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "shore", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "lake", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Lake", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Plant-polinator", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "flower", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "forest", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Pollination", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "fishery", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "river", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "birds", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "salt marsh", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "-pollinator", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "stream", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Current", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Gulf", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "oak", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Sea", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "tributaries", negate = FALSE),]$ecosystem <- "freshwater"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "deep-sea", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "estuary", negate = FALSE),]$ecosystem <- "coastal"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "insect", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "marine", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "kelp", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "pine logs", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "Insect", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "anemonefish", negate = FALSE),]$ecosystem <- "marine"
final_data_frame_2[stringr::str_detect(final_data_frame_2$network_description, "fruit", negate = FALSE),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "southern California", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "pond", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Bay", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Laguna", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Loch", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "River", negate = FALSE)) & is.na(final_data_frame_2$ecosystem) ,]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "prairie", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "reef", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "bird", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "lagoon", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "freshwater", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "atoll", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Aleutian", negate = FALSE)),]$ecosystem <- "marine"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "edge", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Edge", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Malaysia", negate = FALSE)),]$ecosystem[2:3] <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "grassland", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "grassland", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "soybean", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "primate", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "riffle community", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Oak galls", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "rabbit carrion", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "dung", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "N. pervillei", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Akatore", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "alkalin thermal spring ecosystem", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Itaipu Reservoir", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "springbrook community", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Australian shelf", negate = FALSE)),]$ecosystem <- "coastal"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "sand-bottom community", negate = FALSE)),]$ecosystem <- "marine"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Ria de Vigo", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "The food web structure of N. distillatoria in Sri Lanka", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "The food web structure of N. madagascarensis in Madagascar", negate = FALSE)),]$ecosystem <- "freshwater"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Himalayas", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Aspen Parkland", negate = FALSE)),]$ecosystem <- "terrestrial"
final_data_frame_2[(stringr::str_detect(final_data_frame_2$network_description, "Food web of the oligohaline systeme", negate = FALSE)),]$ecosystem <- "coastal"

#Search one by one
sum(is.na(final_data_frame_2$ecosystem)) #How many missing?
#final_data_frame_2[is.na(final_data_frame_2$ecosystem),]$network_description
#final_data_frame_2[is.na(final_data_frame_2$ecosystem),]$doi
#View(final_data_frame_2)
names(final_data_frame_2)

final_data_frame_3 <- final_data_frame_2[!is.na(final_data_frame_2$ecosystem),]
#final_data_frame_3[is.na(final_data_frame_3$ecosystem),]$doi

################################################################################
# Remove Marine Networks
################################################################################

final_data_frame_4 <- final_data_frame_3[!final_data_frame_3$ecosystem == "marine",]
#names(final_data_frame_4)
#View(final_data_frame_4)

################################################################################
# Remove non-directed graphs
################################################################################

is_directed <- c(unlist(lapply(mutualistic_networks_igraph, is.directed)),
                 unlist(lapply(antagonistic_networks_igraph, is.directed)))

net_id_is_directed_mut <- c()
net_id_is_directed_ant <- c()

for(i in 1:length(mutualistic_networks)){
  
  net_id_is_directed_mut[i] <- paste0("Network #", mutualistic_networks[[i]]$network$network_id)
  
}
#
for(i in 1:length(antagonistic_networks)){
  
  net_id_is_directed_ant[i] <- paste0("Network #", antagonistic_networks[[i]]$network$network_id)
  
}

net_id_is_directed <- c(net_id_is_directed_mut, net_id_is_directed_ant)

table(final_data_frame_4$network_number %in% net_id_is_directed)

################################################################################
#Select by year - above 1980
################################################################################

final_data_frame_5 <- final_data_frame_4[(final_data_frame_4$year > 1980),]
#View(final_data_frame_5)
#nrow(final_data_frame_5)

################################################################################
# Create geographic data frame
################################################################################

#Load world shapefile
world <- terra::vect("D:/sig/world_continents.shp")

final_data_frame_6 <- final_data_frame_5[!is.na(final_data_frame_5$x),]

#Create Spatial Points Data Frame
final_data_frame_6_SPATIAL <- vect(final_data_frame_6, geom = c("x", "y"), 
                                   crs = crs(world), keepgeom = FALSE)


plot(world)
plot(final_data_frame_6_SPATIAL, add=TRUE)
#nrow(final_data_frame_6_SPATIAL)
