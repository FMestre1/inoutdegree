
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



