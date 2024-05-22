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
library(mgcv)
library(easystats)
#?mgcv::gam
#install.packages("remotes")
#remotes::install_github("samclifford/mgcv.helper")
library(mgcv.helper)
library(ggplot2)


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
plot(test_data_final_data_frame_10_ANT$distance, as.vector(predict_rforest_FW))

# Evaluate the accuracy (for regression, you might use other metrics)
accuracy_fw <- sqrt(mean((as.vector(predict_rforest_FW) - test_data_final_data_frame_10_ANT$distance)^2, na.rm = TRUE))
cat("Root Mean Squared Error:", accuracy_fw, "\n")

randomForest::varImpPlot(rforest_FW) #Variable importance
#randomForest::treesize(rforest_FW) # Tree size

fw_predicts_DF <- data.frame(final_data_frame_10_ANT, as.vector(predict(rforest_FW, final_data_frame_10_ANT)))
names(fw_predicts_DF)[8] <- "predicts"

ggplot(fw_predicts_DF, aes(bio4, predicts)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x + I(x^2), se = FALSE)

#AQUI

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
plot(caret::varImp(bagged_cv_MUT))  

#predict on test dataset
red_mut <- predict(bagged_cv_MUT, final_data_frame_10_MUT_test)
RMSE(red_mut, final_data_frame_10_MUT_test[complete.cases(final_data_frame_10_MUT_test),]$distance)

################################################################################
################################################################################
