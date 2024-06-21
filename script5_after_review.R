

################################################################################
#                             Code ran after review
################################################################################

#FMestre
#29-04-2024

#Load packages
library(devtools)
library(stringr)
library(writexl)
library(randomForest)
#?randomForest::randomForest
#remotes::install_github('munoztd0/reprtree')
library(reprtree)
#library(mgcv)
library(easystats)
#?mgcv::gam
#install.packages("remotes")
#remotes::install_github("samclifford/mgcv.helper")
library(mgcv.helper)
library(ggplot2)
library(caret)


################################################################################
#                              USE THESE DATA FILES
################################################################################

#Load
#read.csv("final_data_frame_10_ANT_09_05_2024.csv")
#read.csv("final_data_frame_10_MUT_09_05_2024.csv")
#read.csv("responses_MUT_09_05_2024.csv")
#read.csv("final_data_frame_9_MUT_09_05_2024.csv")
#read.csv("responses_ANT_09_05_2024.csv")
#read.csv("final_data_frame_9_ANT_09_05_2024.csv")


################################################################################
#                             RUN RANDOM FOREST
################################################################################

#randomForest::rfcv(trainx = final_data_frame_10_ANT[,2:7],
#                   trainy = final_data_frame_10_ANT$distance,
#                   cv.fold = 10,
#                   step = 1) # ERROR!!


##### Random Forest #####
#?randomForest

#Divide train and test datasets
# Split the dataset into training and testing sets
set.seed(123)

train_indices <- sample(1:nrow(final_data_frame_10_ANT), 0.7 * nrow(final_data_frame_10_ANT))
train_data_final_data_frame_10_ANT <- final_data_frame_10_ANT[train_indices, ]
test_data_final_data_frame_10_ANT <- final_data_frame_10_ANT[-train_indices, ]

# 1.1. Food webs
rforest_FW <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           importance=TRUE,
                           na.action = na.omit,
                           data = train_data_final_data_frame_10_ANT,
                           ntree=1000,
                           keep.forest=TRUE
                           )

plot(rforest_FW)

#A ntree=600 is good enough
rforest_FW <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           importance=TRUE,
                           na.action = na.omit,
                           data = train_data_final_data_frame_10_ANT,
                           ntree=600,
                           keep.forest=TRUE
)

plot(rforest_FW)


predict_rforest_FW <- predict(rforest_FW, test_data_final_data_frame_10_ANT)
#Preditec versus obseved
plot(test_data_final_data_frame_10_ANT$distance, as.vector(predict_rforest_FW))

# Evaluate the accuracy (for regression, you might use other metrics)
accuracy_fw <- sqrt(mean((as.vector(predict_rforest_FW) - test_data_final_data_frame_10_ANT$distance)^2, na.rm = TRUE))
cat("Root Mean Squared Error:", accuracy_fw, "\n")

randomForest::varImpPlot(rforest_FW) #Variable importance
#randomForest::treesize(rforest_FW) # Tree size

fw_predicts_DF <- data.frame(final_data_frame_10_ANT, as.vector(predict(rforest_FW, final_data_frame_10_ANT)))
names(fw_predicts_DF)[8] <- "predicts"

#Relate predictions with some variables
ggplot(fw_predicts_DF, aes(bio4, predicts)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x + I(x^2), se = FALSE)


# 2.1. Mutualistic Networks
rforest_MUT <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           importance=TRUE,
                           proximity=TRUE,
                           na.action=na.omit,
                           data = final_data_frame_10_MUT)

# 2.2. Variable importance
randomForest::varImpPlot(rforest_MUT)


################################################################################
#                     Random Forest 70%-30% (train/test)
################################################################################

# Specify 10-fold cross validation
ctrl <- caret::trainControl(method = "LOOCV",  number = 10) 

##### ANT #####

# Split dataset: train/test
set.seed(123)

final_data_frame_10_ANT_split <- rsample::initial_split(final_data_frame_10_ANT, prop = .7)
final_data_frame_10_ANT_train <- rsample::training(final_data_frame_10_ANT_split)
final_data_frame_10_ANT_test  <- rsample::testing(final_data_frame_10_ANT_split)

# CV bagged model
bagged_cv_FW <- caret::train(
  form = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
  data = final_data_frame_10_ANT_train,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE,
  na.action = na.omit
)

#Plot Var Importance
plot(caret::varImp(bagged_cv_FW))  

#predict on test dataset
red_fw <- predict(bagged_cv_FW, final_data_frame_10_ANT_test)
RMSE(red_fw, final_data_frame_10_ANT_test[complete.cases(final_data_frame_10_ANT_test),]$distance)


##### MUT #####

# Split dataset: train/test
set.seed(123)

final_data_frame_10_MUT_split <- rsample::initial_split(final_data_frame_10_MUT, prop = .7)
final_data_frame_10_MUT_train <- rsample::training(final_data_frame_10_MUT_split)
final_data_frame_10_MUT_test  <- rsample::testing(final_data_frame_10_MUT_split)

# CV bagged model
bagged_cv_MUT <- caret::train(
  form = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
  data = final_data_frame_10_MUT_train,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE,
  na.action = na.omit
)

#Plot Var Importance


#predict on test dataset
red_mut <- predict(bagged_cv_MUT, final_data_frame_10_MUT_test)
RMSE(red_mut, final_data_frame_10_MUT_test[complete.cases(final_data_frame_10_MUT_test),]$distance)

################################################################################
#                                      GAM
################################################################################

#FMestre
#05-06-2024
#citation("mgcv")

#final_data_frame_9_ANT
#final_data_frame_9_MUT

#Getting information on lat long
final_data_frame_9_shp <- terra::vect("C:/Users/asus/Documents/0. Artigos/4. SUBMETIDOS/in_out_degree/shapes/final_data_frame_9.shp")
final_data_frame_9_shp <- as.data.frame(final_data_frame_9_shp)

final_data_frame_9_ANT_v2 <- data.frame(final_data_frame_9_ANT, NA, NA)
names(final_data_frame_9_ANT_v2)[43:44] <- c("lat", "long")

for(i in 1:nrow(final_data_frame_9_ANT_v2)){
  
  row_ANT <- final_data_frame_9_ANT_v2[i,]
  row_ANT_network_number <- row_ANT$network_number
  row_ANT_dataset_id <- row_ANT$dataset_id
  df0 <- final_data_frame_8_shp[final_data_frame_8_shp$network_n0 == row_ANT_network_number & final_data_frame_8_shp$dataset_id == row_ANT_dataset_id,]
  final_data_frame_9_ANT_v2$lat[i] <- df0$lat
  final_data_frame_9_ANT_v2$long[i] <- df0$long
  
}

##

final_data_frame_9_MUT_v2 <- data.frame(final_data_frame_9_MUT, NA, NA)
names(final_data_frame_9_MUT_v2)[42:43] <- c("lat", "long")

for(i in 1:nrow(final_data_frame_9_MUT_v2)){
  
  row_MUT <- final_data_frame_9_MUT_v2[i,]
  row_MUT_network_number <- row_MUT$network_number
  row_MUT_dataset_id <- row_MUT$dataset_id
  df1 <- final_data_frame_8_shp[final_data_frame_8_shp$network_n0 == row_MUT_network_number & final_data_frame_8_shp$dataset_id == row_MUT_dataset_id,]
  final_data_frame_9_MUT_v2$lat[i] <- df1$lat
  final_data_frame_9_MUT_v2$long[i] <- df1$long
  
}


#?mgcv::gam

gam_fw <- mgcv::gam(distance ~ s(bio12)+s(bio15)+s(solar_radiation)+s(human_footprint), correlation=corGaus(1,form=~lat+long),
                    data= final_data_frame_9_ANT_v2,
                    family = gaussian,
                    methods = "REML"
)

summary(gam_fw)
as.vector(predict(gam_fw, final_data_frame_9_ANT_v2))
mgcv::plot.gam(gam_fw)

#Plot


#####

gam_mut <- mgcv::gam(distance ~ s(bio12)+s(bio15)+s(solar_radiation)+s(human_footprint), correlation=corGaus(1,form=~lat+long),
                    data= final_data_frame_9_MUT_v2,
                    family = gaussian,
                    methods = "REML"
)

summary(gam_mut)
as.vector(predict(gam_mut, final_data_frame_9_MUT_v2))



