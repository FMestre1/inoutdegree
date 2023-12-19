################################################################################
# Experimenting using all the MERRAClim variables
################################################################################

#Separate MUT and ANT
final_data_frame_9_ANT <- final_data_frame_8_SPATIAL[final_data_frame_8_SPATIAL$type == "antagonistic",]
#nrow(final_data_frame_9_ANT)

final_data_frame_9_MUT <- final_data_frame_8_SPATIAL[final_data_frame_8_SPATIAL$type == "mutualistic",]
#nrow(final_data_frame_9_MUT)
#names(final_data_frame_9_ANT)

rpart_FW2 <- rpart::rpart(distance ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + 
                            bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 +
                            bio15 + bio16 + bio17 + bio18 + bio19 + solar_radiation +                
                            human_footprint, data = final_data_frame_9_ANT)

#
rpart_MUT2 <- rpart::rpart(distance ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + 
                             bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 +
                             bio15 + bio16 + bio17 + bio18 + bio19 + solar_radiation +                
                             human_footprint, data = final_data_frame_9_MUT)



ptree_rpart_FW2 <- prune(rpart_FW2,
                        cp = rpart_FW2$cptable[which.min(rpart_FW2$cptable[,"xerror"]),"CP"])

ptree_rpart_MUT2 <- prune(rpart_MUT2,
                         cp = rpart_MUT2$cptable[which.min(rpart_MUT2$cptable[,"xerror"]),"CP"])


#Plot
fancyRpartPlot(ptree_rpart_FW2, cex = 0.5)
fancyRpartPlot(ptree_rpart_MUT2, cex = 0.5)


#Statistics
#Univariate - MUT
plotcp(ptree_rpart_MUT2)
printcp(ptree_rpart_MUT2)
summary(ptree_rpart_MUT2)
rsq.rpart(ptree_rpart_MUT2)

printcp_MUT_pruned2 <- printcp(ptree_rpart_MUT2)
rsq.val_MUT_pruned2 <- 1-printcp_MUT_pruned2[,c(3,4)] 
rsq.val_MUT_pruned2[nrow(rsq.val_MUT_pruned2),]
round(as.numeric(rsq.val_MUT_pruned2[nrow(rsq.val_MUT_pruned2),][1]),3)*100
#
#Univariate - ANT
plotcp(ptree_rpart_FW2)
printcp(ptree_rpart_FW2)
summary(ptree_rpart_FW2)
rsq.rpart(ptree_rpart_FW2)

printcp_ANT_pruned2 <- printcp(ptree_rpart_FW2)
rsq.val_ANT_pruned2 <- 1-printcp_ANT_pruned2[,c(3,4)] 
rsq.val_ANT_pruned2[nrow(rsq.val_ANT_pruned2),]
round(as.numeric(rsq.val_ANT_pruned2[nrow(rsq.val_ANT_pruned2),][1]),3)*100

