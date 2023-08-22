################################################################################
#                              Plotting the trees
################################################################################

#FMestre
#08-03-2023

#The trees that I have
rpart_MUT_3
rpart_FW_3

#Create latitude raster
# Load the original raster
#canuse solar radiation raster

# Get the latitude values for each cell in the original raster
lat_values <- xyFromCell(solar_radiation, 1:ncell(solar_radiation))
lat_values <- lat_values[,2]

# Create a new raster with the latitude values
lat_raster <- raster(solar_radiation)
values(lat_raster) <- matrix(lat_values, nrow=nrow(solar_radiation), ncol=ncol(solar_radiation), byrow=TRUE)
plot(lat_raster)

lat_raster <- mask(lat_raster, solar_radiation)

#Antagonistic
solar_radiation
bio4
h_footprint
lat_raster


# Define the target CRS
target_crs <- crs(solar_radiation)

# Project the raster to the target CRS
h_footprint2 <- projectRaster(h_footprint, crs=target_crs)
#plot(h_footprint2)


# Same extent and resolution to create stacks
solar_radiation2 <- crop(solar_radiation, ext)
bio4_2 <- crop(bio4, ext)
lat_raster2 <- crop(lat_raster, ext)
#
h_footprint3 <- raster::resample(h_footprint2, lat_raster2)
bio4_3 <- raster::resample(bio4_2, lat_raster2)

ant_stack <- stack(solar_radiation2, bio4_3, h_footprint3, lat_raster2)
plot(ant_stack)
names(ant_stack) <- c("solar_radiation", "bio4", "human_footprint", "latitude")

#rpart_FW_3

#1
map_rpart_FW_1 <- (ant_stack$solar_radiation >=13278.92) + (ant_stack$bio4 >=7979.5)

#2
map_rpart_FW_2 <- (ant_stack$solar_radiation >= 13278.92) + 
       (ant_stack$bio4 < 7979.5) +
       (ant_stack$human_footprint >= 6.84328) +
       (ant_stack$latitude < 31.91314)

#3
map_rpart_FW_3 <- (ant_stack$solar_radiation >= 13278.92) + 
       (ant_stack$bio4 < 7979.5) +
       (ant_stack$human_footprint >= 6.84328) +
       (ant_stack$latitude >= 31.91314)

#4
map_rpart_FW_4 <- (ant_stack$solar_radiation >= 13278.92) + 
       (ant_stack$bio4 < 7979.5) +
       (ant_stack$human_footprint < 6.84328) +
       (ant_stack$solar_radiation >= 15943.67)

#5
map_rpart_FW_5 <- (ant_stack$solar_radiation >= 13278.92) + 
       (ant_stack$bio4 < 7979.5) +
       (ant_stack$human_footprint < 6.84328) +
       (ant_stack$solar_radiation < 15943.67)

#6
map_rpart_FW_6 <- (ant_stack$solar_radiation < 13278.92) + 
       (ant_stack$human_footprint < 30.06707)


#7
map_rpart_FW_7 <- (ant_stack$solar_radiation < 13278.92) + 
       (ant_stack$human_footprint >= 30.06707)

par(mfrow=c(4,2))
plot(map_rpart_FW_1==2, main="1", legend = F)
plot(map_rpart_FW_2==4, main="2", legend = F)
plot(map_rpart_FW_3==4, main="3", legend = F)
plot(map_rpart_FW_4==4, main="4", legend = F)
plot(map_rpart_FW_5==4, main="5", legend = F)
plot(map_rpart_FW_6==2, main="6", legend = F)
plot(map_rpart_FW_7==2, main="7", legend = F)



#Mutualistic
lat_raster
solar_radiation
bio15

extent(bio15)
extent(lat_raster)
bio15 <- crop(bio15, lat_raster)
bio15_3 <- raster::resample(bio15, lat_raster2)


mut_stack <- stack(solar_radiation2, bio15_3, lat_raster2)
plot(mut_stack)
names(mut_stack) <- c("solar_radiation", "bio15", "latitude")


rpart_MUT_3


#1
map_rpart_MUT_1 <- (mut_stack$latitude < -7.614081)

#2
map_rpart_MUT_2 <- (mut_stack$latitude >= -7.614081) + 
  (mut_stack$solar_radiation >= 16015.38) +
  (mut_stack$bio15 < 18003.33)

#3
map_rpart_MUT_3 <- (mut_stack$latitude >= -7.614081) + 
  (mut_stack$solar_radiation >= 16015.38) +
  (mut_stack$bio15 >= 18003.33)

#4
map_rpart_MUT_4 <- (mut_stack$latitude >= -7.614081) + 
  (mut_stack$solar_radiation < 16015.38)

par(mfrow=c(4,1))
plot(map_rpart_MUT_1==1, main="1", legend = F)
plot(map_rpart_MUT_2==3, main="2", legend = F)
plot(map_rpart_MUT_3==3, main="3", legend = F)
plot(map_rpart_MUT_4==2, main="4", legend = F)

################################################################################
#                Combine mutualistic and antagonistic maps
################################################################################
map_rpart_MUT_1[map_rpart_MUT_1!=1] <- 0
map_rpart_MUT_2[map_rpart_MUT_2!=3] <- 0
map_rpart_MUT_3[map_rpart_MUT_3!=3] <- 0
map_rpart_MUT_4[map_rpart_MUT_4!=2] <- 0
#
map_rpart_MUT_1[map_rpart_MUT_1!=0] <- 1
map_rpart_MUT_2[map_rpart_MUT_2!=0] <- 2
map_rpart_MUT_3[map_rpart_MUT_3!=0] <- 3
map_rpart_MUT_4[map_rpart_MUT_4!=0] <- 4

map_rpart_MUT_ALL <- map_rpart_MUT_1+
                     map_rpart_MUT_2+
                     map_rpart_MUT_3+
                     map_rpart_MUT_4

plot(map_rpart_MUT_ALL)

raster::writeRaster(map_rpart_MUT_ALL, filename = "map_rpart_MUT_ALL.TIF")

##

map_rpart_FW_1[map_rpart_FW_1!=2] <- 0
map_rpart_FW_2[map_rpart_FW_2!=4] <- 0
map_rpart_FW_3[map_rpart_FW_3!=4] <- 0
map_rpart_FW_4[map_rpart_FW_4!=4] <- 0
map_rpart_FW_5[map_rpart_FW_5!=4] <- 0
map_rpart_FW_6[map_rpart_FW_6!=2] <- 0
map_rpart_FW_7[map_rpart_FW_7!=2] <- 0


#
map_rpart_FW_1[map_rpart_FW_1!=0] <- 1
map_rpart_FW_2[map_rpart_FW_2!=0] <- 2
map_rpart_FW_3[map_rpart_FW_3!=0] <- 3
map_rpart_FW_4[map_rpart_FW_4!=0] <- 4
map_rpart_FW_5[map_rpart_FW_5!=0] <- 5
map_rpart_FW_6[map_rpart_FW_6!=0] <- 6
map_rpart_FW_7[map_rpart_FW_7!=0] <- 7


map_rpart_FW_ALL <- map_rpart_FW_1 +
  map_rpart_FW_2 +
  map_rpart_FW_3 +
  map_rpart_FW_4 +
  map_rpart_FW_5 +
  map_rpart_FW_6 +
  map_rpart_FW_7
  
plot(map_rpart_FW_ALL)

raster::writeRaster(map_rpart_FW_ALL, filename = "map_rpart_FW_ALL.TIF")

######################################################################################################
# Plot mutualistic per type
######################################################################################################

final_data_frame_15

final_data_frame15_MUTUALISTIC <- final_data_frame_15[final_data_frame_15$type=="mutualistic",]

descript <- final_data_frame15_MUTUALISTIC$network_description

mut_type <- rep(NA, length(descript))

mut_type[!is.na(stringr::str_match(descript, "pollinator"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "polinator"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "Pollination"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "pollinating"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "Pollnator"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "Flower-visiting"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "oil-flowers"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "plant-flower"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "Flower and anthophilous"))] <- "plant-pollinator"
mut_type[!is.na(stringr::str_match(descript, "spring wildflower"))] <- "plant-pollinator"
#
mut_type[is.na(mut_type)] <- "bird-fruit"
#View(cbind(descript, mut_type))

final_data_frame15_MUTUALISTIC_2 <- data.frame(final_data_frame15_MUTUALISTIC, mut_type)
final_data_frame15_MUTUALISTIC_2 <- final_data_frame15_MUTUALISTIC_2[,c("network_number", "mut_type")]

final_data_frame16 <- merge(final_data_frame_15, final_data_frame15_MUTUALISTIC_2, by = "network_number", all = TRUE)

final_data_frame16_SPATIAL <- sp::SpatialPointsDataFrame(coords = final_data_frame16[,7:8], data = final_data_frame16)

rgdal::writeOGR(obj=final_data_frame16_SPATIAL, dsn="final_data_frame16_SPATIAL", layer="final_data_frame16_SPATIAL", driver="ESRI Shapefile")
