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

crs(solar_radiation)
crs(bio4)
crs(h_footprint) 
crs(lat_raster)

# Define the target CRS
target_crs <- crs(solar_radiation)

# Project the raster to the target CRS
h_footprint2 <- projectRaster(h_footprint, crs=target_crs)
plot(h_footprint2)


stack(solar_radiation, bio4, h_footprint, lat_raster)

#Mutualistic
lat_raster
solar_radiation
bio15



library(raster)
library(rpart)
library(rasterVis)

# Load the independent variable rasters
raster1 <- raster("raster1.tif")
raster2 <- raster("raster2.tif")
raster3 <- raster("raster3.tif")

# Create a raster stack or brick with the independent variables
raster_stack <- stack(raster1, raster2, raster3)

# Load the dependent variable raster
response_raster <- raster("response.tif")

# Mask the independent variable raster stack to the extent of the response raster
raster_stack_masked <- mask(raster_stack, response_raster)

# Train the regression tree model
model <- rpart(response ~ ., data=as.data.frame(raster_stack_masked))

# Generate a predicted raster from the model
predicted_raster <- predict(raster_stack, model, progress="text")

# Plot the independent variable rasters and the predicted raster
levelplot(predicted_raster, margin=FALSE, main="Regression Tree Prediction")
levelplot(raster_stack_masked, margin=FALSE, main="Independent Variable Rasters")
