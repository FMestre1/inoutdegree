################################################################################
#                                 PATH ANALSYSIS
################################################################################

#FMestre
#08-11-2022

#Code from:
#https://rpubs.com/tbihansk/302732
#See this also:
#https://advstats.psychstat.org/book/path/index.php
#And:
#https://www.kdnuggets.com/2018/09/introducing-path-analysis-using-r.html

#final_data_frame_14_FW
#final_data_frame_14_MUT

#Load packages
library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
#source('http://openmx.psyc.virginia.edu/getOpenMx.R')

names(final_data_frame_14_FW)


#Standardize variables
final_data_frame_14_FW_standardize <- data.frame(
  final_data_frame_14_FW$cr_ratio_vector,
  final_data_frame_14_FW$sq_wasserstein_in_out_location_PERC,
  final_data_frame_14_FW$sq_wasserstein_in_out_size_PERC,
  final_data_frame_14_FW$sq_wasserstein_in_out_shape_PERC,
  final_data_frame_14_FW$y,
  final_data_frame_14_FW$bio4,
  final_data_frame_14_FW$bio15,
  final_data_frame_14_FW$solar_radiation,
  final_data_frame_14_FW$h_foot_vector
)

names(final_data_frame_14_FW_standardize) <- c(
  "cr_ratio_vector",
  "sq_wasserstein_in_out_location_PERC",
  "sq_wasserstein_in_out_size_PERC",
  "sq_wasserstein_in_out_shape_PERC",
  "y",
  "bio4",
  "bio15",
  "solar_radiation",
  "h_foot_vector"
  )

#Specifiy the model
model_fw <-'
cr_ratio_vector ~ sq_wasserstein_in_out_location_PERC+sq_wasserstein_in_out_size_PERC+sq_wasserstein_in_out_shape_PERC
cr_ratio_vector ~ y+solar_radiation+h_foot_vector+bio4+bio15
#
sq_wasserstein_in_out_size_PERC~~sq_wasserstein_in_out_size_PERC
sq_wasserstein_in_out_location_PERC~~sq_wasserstein_in_out_location_PERC
sq_wasserstein_in_out_shape_PERC~~sq_wasserstein_in_out_shape_PERC
#
sq_wasserstein_in_out_size_PERC ~ y+solar_radiation+h_foot_vector+bio4+bio15
sq_wasserstein_in_out_location_PERC ~ y+solar_radiation+h_foot_vector+bio4+bio15
sq_wasserstein_in_out_shape_PERC ~ y+solar_radiation+h_foot_vector+bio4+bio15
'

#Fit
fit_fw <- lavaan(model_fw, data = final_data_frame_14_FW_standardize)

#Summary
summary(fit_fw, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)

#Plot
semPlot::semPaths(fit_fw, "par",
                  sizeMan = 15, 
                  sizeInt = 15, 
                  sizeLat = 15,
                  edge.label.cex=1.5,
                  fade=FALSE
                  )
