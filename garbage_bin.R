
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

