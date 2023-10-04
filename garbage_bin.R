
library(rpart)  ######

names(final_data_frame_13_MUT)

rpart_MUT <- rpart(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, data = final_data_frame_13_MUT)
library("rpart.plot") ######
rpart.plot::rpart.plot(rpart_MUT)
summary(rpart_MUT)

rsq.rpart(rpart_MUT)	


rpart_FW <- rpart(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, data = final_data_frame_13_FW)

rpart.plot::rpart.plot(rpart_FW)
summary(rpart_FW)

printcp(rpart_FW)


library(rms)
rms::validate(rpart_FW)


1-(0.001679123/var(final_data_frame_13_FW$sq_wasserstein_in_out_distance))
1-(0.005194555/var(final_data_frame_13_MUT$sq_wasserstein_in_out_distance))




library(party) ######

rpart_MUT_2 <- ctree(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, 
                     data = final_data_frame_13_MUT
)
plot(rpart_MUT_2)


rpart_FW_2 <- ctree(sq_wasserstein_in_out_distance ~ bio1+bio4+bio12+bio15+solar_radiation+h_foot_vector+y, 
                    data = final_data_frame_13_FW
)
plot(rpart_FW_2)

################################################################################

#Spatial autocorr
require(adespatial)

#final_data_frame_13_MUT
autocorr_MUT <- adespatial::dbmem(
  xyORdist = final_data_frame_13_MUT[,7:8],
  MEM.autocor = "positive"
)

#final_data_frame_13_FW
autocorr_FW <- adespatial::dbmem(
  xyORdist = final_data_frame_13_FW[,7:8],
  MEM.autocor = "positive"
)


################################################################################


##Second try...####################

require(vegan)
bio_MUT=final_data_frame_13_MUT[,38:56]
pca=rda(bio_MUT)
biplot(pca, scaling = "symmetric", type = c("text", "points"))
summary(pca)
pca.scores=scores(pca)

#extracting values per site
pca.networks=pca.scores$sites

#extracting first axes per site
PCA1_MUT=pca.networks[,1];PCA1
PCA2_MUT=pca.networks[,2];PCA2


mvpart(
  responses_MUT ~ PCA1_MUT+PCA2_MUT+solar_radiation+h_foot_vector+y+ecosystem, 
  data = final_data_frame_13_MUT,
  xv = "min",
  xval = nrow(responses_MUT), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE,  # give "rsq" plot
  pca = TRUE,  # plot PCA of group means and add species and site information
  wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)

##

bio_FW=final_data_frame_13_FW[,38:56]
pca_FW=rda(bio_FW)
biplot(pca_FW, scaling = "symmetric", type = c("text", "points"))
summary(pca_FW)
pca.scores_FW=scores(pca_FW)

#extracting values per site
pca.networks_FW=pca.scores_FW$sites

#extracting first axes per site
PCA1_FW=pca.networks_FW[,1];PCA1
PCA2_FW=pca.networks_FW[,2];PCA2


mvpart(
  responses_FW ~ PCA1_FW+PCA2_FW+solar_radiation+h_foot_vector+y+ecosystem, 
  data = final_data_frame_13_FW,
  xv = "min",
  xval = nrow(responses_FW), # number of cross-validations
  xvmult = 100, # number of multiple cross-validations
  all.leaves = TRUE,  # annotate all nodes
  rsq = TRUE,  # give "rsq" plot
  pca = TRUE,  # plot PCA of group means and add species and site information
  wgt.ave.pca = TRUE  # plot weighted averages across sites for species
)


model_fw <-'
#cr_ratio_vector ~ sq_wasserstein_in_out_location_PERC+sq_wasserstein_in_out_size_PERC+sq_wasserstein_in_out_shape_PERC
cr_ratio_vector ~ y+solar_radiation+h_foot_vector+bio4+bio15
#
#sq_wasserstein_in_out_size_PERC~~sq_wasserstein_in_out_size_PERC
#sq_wasserstein_in_out_location_PERC~~sq_wasserstein_in_out_location_PERC
#sq_wasserstein_in_out_shape_PERC~~sq_wasserstein_in_out_shape_PERC
#
sq_wasserstein_in_out_size_PERC ~ y+solar_radiation+h_foot_vector+bio4+bio15+cr_ratio_vector
sq_wasserstein_in_out_location_PERC ~ y+solar_radiation+h_foot_vector+bio4+bio15+cr_ratio_vector
sq_wasserstein_in_out_shape_PERC ~ y+solar_radiation+h_foot_vector+bio4+bio15+cr_ratio_vector
'
##########
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

###########################################################################
#TEST OTHER DATABASES
library(FWebs)

#EcoBase
eb1 <- create.fw.list(db ="eb", ecosyst = TRUE, spatial = TRUE, ref = TRUE)
eb2 <- remove.non.numeric(eb1)

names(eb1) 
names(eb2)
nrow(eb1$references)

#GlobalWeb
gw1 <- create.fw.list(db ="gw", folder="globalweb", ecosyst = TRUE, ref = TRUE, code = TRUE)

names(gw1)
gw1$int_matrix
View(gw1$int_matrix[[1]])
gw1$ecosystem
View(gw1$references)
gw1$code

gw2 <- remove.repeated.names(gw1)
gw3 <- remove.non.numeric(gw2)
gw4 <- rect2square(gw3)

names(gw2)
names(gw3)
###########################################################################

dd.fw(eb2, log=TRUE, cumulative=TRUE)
dd.fw(gw4, log=TRUE, cumulative=TRUE)



#WEB OF LIVE ##############################################################
#"This work has used the Web of Life dataset (www.web-of-life.es)".
#This does not preclude citing the author of individual networks when needed. 
#You will find the references inside the file references.csv.

############################
#correct the first matrix (it has a "number of sampled" column!!!!!)
matrix1 <- read.csv("C:/Users/FMest/Documents/0. Artigos/in_out_degree/w_life_database/A_HP_001.csv")
View(matrix1)
matrix1 <- matrix1[,-2]
write.csv(matrix1,"C:/Users/FMest/Documents/0. Artigos/in_out_degree/w_life_database/A_HP_001.csv", row.names = FALSE)
############################

wl1 <- create.fw.list(folder="C:/Users/FMest/Documents/0. Artigos/in_out_degree/w_life_database/", 
                      db="wl", ref=TRUE, spatial = TRUE, code = TRUE)

names(wl1)
wl1$code
length(wl1$int_matrix) 
wl1$int_matrix
head(wl1$references)
wl1$spatial_info
names(wl1)
names(wl1$int_matrix)

save(wl1, file="web_of_life_dataset.RData")

##

is.sq.matrix(mg1)
mangal_metrics <- fw.metrics(mg1)
names(mangal_metrics)
#
mg2 <- convert.to.graph.list(mg1)

##

is.sq.matrix(wl1)

#as.vector(unlist(lapply(web_of_life_interaction_matrices, ncol)))
#as.vector(unlist(lapply(web_of_life_interaction_matrices, nrow)))[166]

wl2 <- rect2square(wl1)
web_of_life_metrics <- fw.metrics(wl2)
#
wl3 <- convert.to.graph.list(wl2)



library(FWebs)

#Getting data in ##########################################################

#MANGAL ###################################################################

mangal_1 <- create.fw.list(db="mg", ref=TRUE, spatial = TRUE, code =TRUE, 
                           mangal_types =c("mutualism", "predation", "herbivory", 
                                           "scavenger", "detritivore"))


names(mangal_1$int_matrix)
View(mangal_1$references)
mangal_1$spatial_info
names(mangal_1)
nrow(mangal_1$references)
length(mangal_1$int_matrix)
mangal_1$code

#save(mangal_1, file="mangal_dataset.RData")

###########################################################################


###############################################################3
#Plot in-degree
for(i in 1:length(in_degree_list)){
  
  if(i==1) plot(in_degree_list[[i]], col ="red", type="l", lwd=2, ylim=c(0, 1))
  
  if(i!=1) lines(in_degree_list[[i]], col = sample(rainbow(100),1), type="l", lwd=2)
  
}

#plot out-degree
for(i in 1:length(out_degree_list)){
  
  if(i==1) plot(out_degree_list[[i]], col ="red", type="l", lwd=2, ylim=c(0, 1))
  
  if(i!=1) lines(out_degree_list[[i]], col = sample(rainbow(100),1), type="l", lwd=2)
  
}

#################################################################

