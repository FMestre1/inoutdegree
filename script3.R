################################################################################
# Derive Wasserstein distance
################################################################################

sq_wasserstein_in_out_distance <- c()
sq_wasserstein_in_out_location <- c()
sq_wasserstein_in_out_size <- c()
sq_wasserstein_in_out_shape <- c()

for(i in 1:length(in_degree_list)) {
  
  sq_wasserstein_in_out_distance[i] <- as.numeric(squared_wass_decomp(in_degree_list[[i]], out_degree_list[[i]])[1])
  sq_wasserstein_in_out_location[i] <- as.numeric(squared_wass_decomp(in_degree_list[[i]], out_degree_list[[i]])[2])
  sq_wasserstein_in_out_size[i] <- as.numeric(squared_wass_decomp(in_degree_list[[i]], out_degree_list[[i]])[3])
  sq_wasserstein_in_out_shape[i] <- as.numeric(squared_wass_decomp(in_degree_list[[i]], out_degree_list[[i]])[4])
  
}

#Derive percentages
sq_wasserstein_in_out_location_PERC <- (sq_wasserstein_in_out_location*100)/sq_wasserstein_in_out_distance
sq_wasserstein_in_out_size_PERC <- (sq_wasserstein_in_out_size*100)/sq_wasserstein_in_out_distance
sq_wasserstein_in_out_shape_PERC <- (sq_wasserstein_in_out_shape*100)/sq_wasserstein_in_out_distance

sq_wasserstein_in_out <- data.frame(network_number_list,
                                    sq_wasserstein_in_out_distance,
                                    sq_wasserstein_in_out_location_PERC,
                                    sq_wasserstein_in_out_size_PERC,
                                    sq_wasserstein_in_out_shape_PERC
)

sq_wasserstein_in_out_2 <- sq_wasserstein_in_out[sq_wasserstein_in_out$network_number_list %in% final_data_frame_7_SPATIAL$network_number,]
#sq_wasserstein_in_out_2$network_number_list
#final_data_frame_7_SPATIAL$network_number

final_data_frame_8 <- data.frame(final_data_frame_7_SPATIAL,
                                 data.frame(terra::crds(final_data_frame_7_SPATIAL)),
                                 sq_wasserstein_in_out_2)

final_data_frame_8_SPATIAL <- vect(final_data_frame_8, geom=c("x", "y"), 
                                   crs=crs(world), keepgeom=FALSE)

#Plot
plot(world)
plot(final_data_frame_8_SPATIAL, add=TRUE)
