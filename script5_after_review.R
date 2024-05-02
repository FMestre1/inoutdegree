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

#### FUNCTIONS TO PLOT TREES - START - NOT USED

#From: https://gist.github.com/sillasgonzaga/eef0577c14b83b32f9b7cc480d2765dd
plot_rf_tree <- function(final_model, tree_num, shorten_label = TRUE) {
  
  library(tidyr)
  library(dplyr)
  library(igraph)
  library(ggraph)
  
  # source: https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  
  if (shorten_label) {
    V(graph)$leaf_label <- substr(as.character(tree$prediction), 1, 1)
  }
  
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'tree') + 
    theme_graph() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = FALSE, colour = "white",
                    show.legend = FALSE)
  
  print(plot)
}

#From: https://www.r-bloggers.com/2017/03/plotting-trees-from-random-forest-models-with-ggraph/
tree_func <- function(final_model, tree_num) {
  
  library(dplyr)
  library(ggraph)
  library(igraph)
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

#### FUNCTIONS TO PLOT TREES - END - NOT USED

##### Random Forest #####
#?randomForest

# 1.1. Food webs
rforest_FW <- randomForest(distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint, 
                           importance=TRUE,
                           proximity=TRUE,
                           na.action=na.omit,
                           data = final_data_frame_10_ANT,
                           ntree=1000,
                           keep.forest=TRUE
                           )

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

#?mgcv::gam

gam_fw <- mgcv::gam(distance ~ bio12+bio15+solar_radiation+human_footprint,
                    data= final_data_frame_10_ANT,
                    family = gaussian()
                    )

summary(gam_fw)

#From...
#https://maulikbhatt.quarto.pub/quartopub/posts/Easystats/Easystats.html

#Model paramenters
model_parameters(gam_fw)

#Model performance
model_performance(gam_fw)

#Run checks for the assumptions
check_autocorrelation(gam_fw)
check_collinearity(gam_fw)
check_heteroscedasticity(gam_fw)

#Model report
report(gam_fw)

#Check
#gam.check(b, old.style=FALSE,
#          type=c("deviance","pearson","response"),
#          k.sample=5000,k.rep=200,
#          rep=0, level=.9, rl.col=2, rep.col="gray80", ...)

################################################################################
#                  Bootstrap aggregating (bagging) approach
################################################################################

#From:
#https://uc-r.github.io/regression_trees

#Load library
library(ipred)
library(rsample)
library(caret)

##### ANT  #####

# Train bagged model
bagged_fw <- ipred::bagging(
  formula = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
  data    = final_data_frame_10_ANT,
  coob    = TRUE
)

ntree <- 10:100

# Create empty vector to store RMSE values
rmse_fw <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  
  # reproducibility
  set.seed(123)
  
  # perform bagged model
  bagged_fw <- ipred::bagging(
    formula = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
    data    = final_data_frame_10_ANT,
    coob    = TRUE,
    nbagg   = ntree[i]
    )

  # get OOB error
  rmse_fw[i] <- bagged_fw$err
}

#plot RMSE
plot(ntree, rmse_fw, type = 'l', lwd = 2)
abline(v = 23, col = "red", lty = "dashed")

##### MUT  #####

# Train bagged model
bagged_mut <- ipred::bagging(
  formula = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
  data    = final_data_frame_10_MUT,
  coob    = TRUE
)

ntree <- 10:100

# Create empty vector to store RMSE values
rmse_mut <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  
  # reproducibility
  set.seed(123)
  
  # perform bagged model
  bagged_mut <- ipred::bagging(
    formula = distance ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
    data    = final_data_frame_10_MUT,
    coob    = TRUE,
    nbagg   = ntree[i]
  )
  
  # get OOB error
  rmse_mut[i] <- bagged_mut$err
}

#plot RMSE
plot(ntree, rmse_mut, type = 'l', lwd = 2)

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
