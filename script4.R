################################################################################
# Run the Regression Trees
################################################################################
#12-12-2023

names(final_data_frame_8_SPATIAL)

#Delete some columns
final_data_frame_9 <- final_data_frame_8_SPATIAL[,-c(14,15,16,17,18,39,41)]
#table(final_data_frame_9$type)

#Separate MUT and ANT
final_data_frame_9_ANT <- final_data_frame_9[final_data_frame_9$type == "antagonistic",]
#nrow(final_data_frame_9_ANT)

final_data_frame_9_MUT <- final_data_frame_9[final_data_frame_9$type == "mutualistic",]
#nrow(final_data_frame_9_MUT)
#nrow(final_data_frame_9_ANT) + nrow(final_data_frame_9_MUT)

################################################################################
# Multivariate Regression Tree
################################################################################

#Send files to R version 3.2.0 to run mvpart
write.csv(responses_MUT, file = "responses_MUT_06DEZ23.csv")
write.csv(final_data_frame_9_MUT_3, file = "final_data_frame_9_MUT_3_06DEZ23.csv")
write.csv(responses_ANT, file = "responses_ANT_06DEZ23.csv")
write.csv(final_data_frame_9_ANT_3, file = "final_data_frame_9_ANT_3_06DEZ23.csv")

#R code run in the version 3.2.0 - START

library(mvpart)

responses_MUT <- read.csv("C:\\Users\\asus\\Documents\\responses_MUT_06DEZ23.csv")
final_data_frame_9_MUT <- read.csv("C:\\Users\\asus\\Documents\\final_data_frame_9_MUT_3_06DEZ23.csv")
responses_ANT <- read.csv("C:\\Users\\asus\\Documents\\responses_ANT_06DEZ23.csv")
final_data_frame_9_ANT <- read.csv("C:\\Users\\asus\\Documents\\final_data_frame_9_ANT_3_06DEZ23.csv")

responses_MUT <- as.matrix(responses_MUT)
responses_ANT <- as.matrix(responses_ANT)

responses_MUT <- responses_MUT[,-1]
responses_ANT <- responses_ANT[,-1]

ncol(responses_MUT)
ncol(responses_ANT)

head(responses_MUT)
head(responses_ANT)

colnames(responses_MUT)
colnames(responses_ANT)

MUT_tree <- mvpart::mvpart(
  responses_MUT ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
  data = final_data_frame_9_MUT,
  xv = "pick", #Picking the smallest tree for which the CVRE is within one standard error of the tree with the lowest CVRE (more parsimonious acording to Breiman et al. 1984)
  legend = FALSE,
  xval = nrow(responses_MUT), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE#,  # give "rsq" plot
  #pca = TRUE,  # plot PCA of group means and add species and site information
  #wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

FW_tree <- mvpart::mvpart(
  responses_ANT ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
  data = final_data_frame_9_ANT,
  xv = "pick",
  legend = FALSE,
  xval = nrow(responses_ANT), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE#,  # give "rsq" plot
  #pca = TRUE,  # plot PCA of group means and add species and site information
  #wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

#Summary
summary(MUT_tree)
summary(FW_tree)

#Rsquared
#(1-error)*100

#ANT
tmp_ANT_MULT <- printcp(FW_tree)
rsq.val_ANT_MULT <- 1-tmp_ANT_MULT[,c(3,4)] 
round(as.numeric(rsq.val_ANT_MULT[nrow(rsq.val_ANT_MULT),][1]),3)

#MUT 
tmp_MUT_MULT <- printcp(MUT_tree)
rsq.val_MUT_MULT <- 1-tmp_MUT_MULT[,c(3,4)] 
round(as.numeric(rsq.val_MUT_MULT[nrow(rsq.val_MUT_MULT),][1]),3)

#R code run in the version 3.2.0 - END

################################################################################
# Univariate Regression Tree
################################################################################

final_data_frame_10_ANT <- data.frame(final_data_frame_9_ANT[,c("sq_wasserstein_in_out_distance", "bio1", "bio4", "bio12", "bio15", "wc2.1_10m_srad_01", "wildareas.v3.2009.human.footprint")])
names(final_data_frame_10_ANT)[1] <- "distance"
names(final_data_frame_10_ANT)[6] <- "solar_radiation"
names(final_data_frame_10_ANT)[7] <- "human_footprint"
#
final_data_frame_10_MUT <- data.frame(final_data_frame_9_MUT[,c("sq_wasserstein_in_out_distance", "bio1", "bio4", "bio12", "bio15", "wc2.1_10m_srad_01", "wildareas.v3.2009.human.footprint")])
names(final_data_frame_10_MUT)[1] <- "distance"
names(final_data_frame_10_MUT)[6] <- "solar_radiation"
names(final_data_frame_10_MUT)[7] <- "human_footprint"
#
rpart_FW <- rpart::rpart(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           data = final_data_frame_10_ANT)

#
rpart_MUT <- rpart::rpart(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                            data = final_data_frame_10_MUT)

#How many networks in each type of network
nrow(final_data_frame_10_ANT)
nrow(final_data_frame_10_MUT)

#Plot
fancyRpartPlot(rpart_MUT)
fancyRpartPlot(rpart_FW)

#Code from:
#https://www.edureka.co/blog/implementation-of-decision-tree/

ptree_rpart_FW <- prune(rpart_FW,
              cp = rpart_FW$cptable[which.min(rpart_FW$cptable[,"xerror"]),"CP"])

ptree_rpart_MUT <- prune(rpart_MUT,
              cp = rpart_MUT$cptable[which.min(rpart_MUT$cptable[,"xerror"]),"CP"])

#Statistics
#Univariate - MUT
plotcp(ptree_rpart_MUT)
printcp(ptree_rpart_MUT)
summary(ptree_rpart_MUT)
rsq.rpart(ptree_rpart_MUT)

printcp_MUT_pruned <- printcp(ptree_rpart_MUT)
rsq.val_MUT_pruned <- 1-printcp_MUT_pruned[,c(3,4)] 
rsq.val_MUT_pruned[nrow(rsq.val_MUT_pruned),]
round(as.numeric(rsq.val_MUT_pruned[nrow(rsq.val_MUT_pruned),][1]),3)

#Univariate - ANT
plotcp(ptree_rpart_FW)
printcp(ptree_rpart_FW)
summary(ptree_rpart_FW)
rsq.rpart(ptree_rpart_FW)

printcp_ANT_pruned <- printcp(ptree_rpart_FW)
rsq.val_ANT_pruned <- 1-printcp_ANT_pruned[,c(3,4)] 
rsq.val_ANT_pruned[nrow(rsq.val_ANT_pruned),]
round(as.numeric(rsq.val_ANT_pruned[nrow(rsq.val_ANT_pruned),][1]),3)

#Plot
fancyRpartPlot(ptree_rpart_MUT)
fancyRpartPlot(ptree_rpart_FW)

#Load multivariate trees
#load("FW_tree.RData")
#load("MUT_tree.RData")
#fancyRpartPlot(FW_tree)
#fancyRpartPlot(MUT_tree)

#Load - Save
#save.image(file = "inout_12DEZ23.RData")
#load("inout_12DEZ23.RData")