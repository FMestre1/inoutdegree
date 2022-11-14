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
names(final_data_frame_14_FW)
final_data_frame_14_FW$hanpp_vector

final_data_frame_14_FW_vars <- data.frame(
  final_data_frame_14_FW$network_number,
  scale(final_data_frame_14_FW$nnodes),
  scale(final_data_frame_14_FW$nedges),
  final_data_frame_14_FW$type,
  final_data_frame_14_FW$ecosystem,
  final_data_frame_14_FW$cr_ratio_vector,
  final_data_frame_14_FW$sq_wasserstein_in_out_location_PERC,
  final_data_frame_14_FW$sq_wasserstein_in_out_size_PERC,
  final_data_frame_14_FW$sq_wasserstein_in_out_shape_PERC,
  final_data_frame_14_FW$y,
  scale(final_data_frame_14_FW$bio4),
  scale(final_data_frame_14_FW$bio15),
  scale(final_data_frame_14_FW$solar_radiation),
  scale(final_data_frame_14_FW$h_foot_vector)
)

names(final_data_frame_14_FW_vars) <- c(
  "network_number",
  "nnodes",
  "nedges",
  "type",
  "ecosystem",
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

#View(final_data_frame_14_FW_vars)

##
#Test with glm

#t1 <- glm(cr_ratio_vector ~ y+solar_radiation+h_foot_vector+bio4+bio15,
#          data = final_data_frame_14_FW_vars)
#summary(t1)
#rm(t1)

##

#Specifiy the model
model_fw <- ' 
  ## regressions ##
cr_ratio_vector ~ y+solar_radiation+h_foot_vector+bio4+bio15+nnodes+nedges
nnodes ~ y+solar_radiation+h_foot_vector+bio4+bio15
nedges ~ y+solar_radiation+h_foot_vector+bio4+bio15
##
  ## variances and covariances ##
#cr_ratio_vector ~~ cr_ratio_vector
  ## intercepts ##
#cr_ratio_vector ~ 1
'

#Fit
rm(fit_fw)
fit_fw <- lavaan::sem(model_fw, data = final_data_frame_14_FW_vars)

#Summary
summary(fit_fw, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)

#Plot
semPlot::semPaths(fit_fw, 
                  what = "std",
                  layout = "tree",
                  edge.label.cex = 2.0,
                  label.cex = 2,
                  curvePivot = FALSE, 
                  rotation = 3,
                  fade=TRUE,
                  #whatLabels = "hide",
                  nCharNodes = FALSE
                  )


