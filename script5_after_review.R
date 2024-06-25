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
#library(mgcv.helper)
library(ggplot2)
library(caret)
#library(gratia)
#library(mgcv)

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

#?randomForest

#Divide train and test datasets
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

varimp_fw <- randomForest::varImpPlot(rforest_FW) #Variable importance
varimp_fw <- as.data.frame(randomForest::varImpPlot(rforest_FW)) #Variable importance
varimp_fw <- varimp_fw[order(varimp_fw$`%IncMSE`, decreasing = TRUE),]
varimp_fw <- data.frame(rownames(varimp_fw), varimp_fw)
names(varimp_fw) <- c("variable_name", "IncMSE", "IncNodePurity")

ggplot(varimp_fw) +
  geom_col(aes(x = reorder(variable_name, -IncMSE), y = IncMSE), fill = "dodgerblue2", width = 0.3) +
  xlab("Variable name") +
  ylab("% IncMSE") +
  coord_flip()

###

train_indices2 <- sample(1:nrow(final_data_frame_10_MUT), 0.7 * nrow(final_data_frame_10_MUT))
train_data_final_data_frame_10_MUT <- final_data_frame_10_MUT[train_indices2, ]
test_data_final_data_frame_10_MUT <- final_data_frame_10_MUT[-train_indices2, ]

# 1.2. Mutualistic networks
rforest_MUT <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           importance=TRUE,
                           na.action = na.omit,
                           data = train_data_final_data_frame_10_MUT,
                           ntree=1000,
                           keep.forest=TRUE
)

plot(rforest_MUT)

rforest_MUT <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                            importance=TRUE,
                            na.action = na.omit,
                            data = train_data_final_data_frame_10_MUT,
                            ntree=600,
                            keep.forest=TRUE
)

plot(rforest_MUT)

predict_rforest_MUT <- predict(rforest_MUT, test_data_final_data_frame_10_MUT)
#Preditec versus obseved
plot(test_data_final_data_frame_10_MUT$distance, as.vector(predict_rforest_MUT))

# Evaluate the accuracy (for regression, you might use other metrics)
accuracy_mut <- sqrt(mean((as.vector(predict_rforest_MUT) - test_data_final_data_frame_10_MUT$distance)^2, na.rm = TRUE))
cat("Root Mean Squared Error:", accuracy_mut, "\n")

varimp_mut <- as.data.frame(randomForest::varImpPlot(rforest_MUT)) #Variable importance
varimp_mut <- varimp_mut[order(varimp_mut$`%IncMSE`, decreasing = TRUE),]
varimp_mut <- data.frame(rownames(varimp_mut), varimp_mut)
names(varimp_mut) <- c("variable_name", "IncMSE", "IncNodePurity")

ggplot(varimp_mut) +
  geom_col(aes(x = reorder(variable_name, -IncMSE), y = IncMSE), fill = "dodgerblue2", width = 0.3) +
  xlab("Variable name") +
  ylab("% IncMSE") +
  coord_flip()


