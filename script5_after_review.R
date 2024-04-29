################################################################################
#                             Code ran after review
################################################################################

#FMestre
#29-04-2024

#Load packages
library(devtools)
library(stringr)
library(writexl)
##### Loading packages #####
library(randomForest)
#?randomForest::randomForest
#remotes::install_github('munoztd0/reprtree')
library(reprtree)

################################################################################
#                             GENERATE REFERENCE LIST
################################################################################

#1. References for antagonistic networks

ref_ANT <- data.frame(matrix(ncol = 4))
names(ref_ANT) <- c("Dataset_ID", "Network_ID", "Reference", "DOI")

for(i in 1:length(final_data_frame_9_ANT$doi)){
#ref1 <- rmangal::search_references(doi = final_data_frame_9_ANT$doi[1]
ref_ANT[i,1] <- final_data_frame_9_ANT$dataset_id[i]
ref_ANT[i,2] <- final_data_frame_9_ANT$network_number[i]
ref1 <- RefManageR::GetBibEntryWithDOI(final_data_frame_9_ANT$doi[i])
ref_ANT[i,3] <- format(ref1)
ref_ANT[i,4] <- final_data_frame_9_ANT$doi[i]

message(i)
}

ref_ANT <- data.frame(ref_ANT, rep("antagonistic_network", nrow(ref_ANT)))
names(ref_ANT)[5] <- "Network_Type"

#2. References for mutualistic networks
ref_MUT <- data.frame(matrix(ncol = 4))
names(ref_MUT) <- c("Dataset_ID", "Network_ID", "Reference", "DOI")

for(i in 1:length(final_data_frame_9_MUT$doi)){
  #ref1 <- rmangal::search_references(doi = final_data_frame_9_ANT$doi[1]
  ref_MUT[i,1] <- final_data_frame_9_MUT$dataset_id[i]
  ref_MUT[i,2] <- final_data_frame_9_MUT$network_number[i]
  ref2 <- RefManageR::GetBibEntryWithDOI(stringr::str_trim(final_data_frame_9_MUT$doi[i]))
  ref_MUT[i,3] <- format(ref2)
  ref_MUT[i,4] <- final_data_frame_9_MUT$doi[i]
  
  message(i)
}

ref_MUT <- data.frame(ref_MUT, rep("mutualistic_network", nrow(ref_MUT)))
names(ref_MUT)[5] <- "Network_Type"

#3. Build overall table
reference_list <- rbind(ref_ANT, ref_MUT)
#
reference_list$Dataset_ID <- as.numeric(stringr::str_extract(reference_list$Dataset_ID, "\\d+"))
reference_list$Network_ID <- as.numeric(stringr::str_extract(reference_list$Network_ID, "\\d+"))
reference_list$Reference <- stringr::str_remove(reference_list$Reference, "\\[1\\] ")

#Verify the reference at:
#https://mangal.io/api/v2/network/

#4. save it as an Excel file
#writexl::write_xlsx(reference_list, "reference_list.xlsx")

################################################################################
#                             RUN RANDOM FOREST
################################################################################

#randomForest::rfcv(trainx = final_data_frame_10_ANT[,2:7],
#                   trainy = final_data_frame_10_ANT$distance,
#                   cv.fold = 10,
#                   step = 1) # ERROR!!

##### Random Forest #####

# 1.1. Food webs
rforest_FW <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           importance=TRUE,
                           proximity=TRUE,
                           na.action=na.omit,
                           data = final_data_frame_10_ANT)

# 1.2. Variable importance
randomForest::varImpPlot(rforest_FW)
#reprtree:::plot.getTree(rforest_FW, k=3, depth=4)


# 2.1. Mutualistic Networks
rforest_MUT <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           importance=TRUE,
                           proximity=TRUE,
                           na.action=na.omit,
                           data = final_data_frame_10_MUT)

# 2.2. Variable importance
randomForest::varImpPlot(rforest_MUT)


################################################################################
#                                     GAM
################################################################################

library(mgcv)
library(easystats)
#?mgcv::gam
#install.packages("remotes")
#remotes::install_github("samclifford/mgcv.helper")
library(mgcv.helper)

gam_fw <- mgcv::gam(distance ~ bio12+bio15+solar_radiation+human_footprint,
                    data= final_data_frame_10_ANT
                    )

#VIF
#?vif.gam
vif.gam(gam_fw)

#From...
#https://maulikbhatt.quarto.pub/quartopub/posts/Easystats/Easystats.html

#Model paramenters
model_parameters(gam_fw)

#Model performance
model_performance(gam_fw)

#Run checks
check_autocorrelation(gam_fw)
check_collinearity(gam_fw)
check_heteroscedasticity(gam_fw)
report(gam_fw)

#Check
#gam.check(b, old.style=FALSE,
#          type=c("deviance","pearson","response"),
#          k.sample=5000,k.rep=200,
#          rep=0, level=.9, rl.col=2, rep.col="gray80", ...)
