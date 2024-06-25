################################################################################
#Compare with previous df
################################################################################
#
#load("C://Users//asus//Documents//github//inoutdegree//final_data_frame_14_PREV_06DEZ23.RData")
final_data_frame_14_PREV$network_number

#PREVIOUS
View(t(final_data_frame_14_PREV[final_data_frame_14_PREV$network_number == "Network #91",]))
#View(final_data_frame_14_PREV)
#names(final_data_frame_14_PREV)
nrow(final_data_frame_14_PREV)

#Are there NA in the distances?
final_data_frame_14_PREV$sq_wasserstein_in_out_location_PERC

#CURRENT
#final_data_frame_9_df <- data.frame(final_data_frame_9)
View(t(final_data_frame_9_df[final_data_frame_9_df$network_number == "Network #91",]))
nrow(final_data_frame_9_df)

#Are there NA in the distances?
final_data_frame_9_df$sq_wasserstein_in_out_location_PERC

#Look at the same variable dependent in both
par(mfrow = c(1, 2))
hist(final_data_frame_14_PREV$sq_wasserstein_in_out_location_PERC, ylim = c(0,100))
hist(final_data_frame_9_df$sq_wasserstein_in_out_location_PERC, ylim = c(0,100))

#Look at the same variable independent in both
par(mfrow = c(1, 2))
hist(final_data_frame_14_PREV$solar_radiation, ylim = c(0,150))
hist(final_data_frame_9_df$wc2.1_10m_srad_01, ylim = c(0,150))

#write shapefile and rasters to verify in QGIS
#writeVector(final_data_frame_9, "final_data_frame_9.shp", overwrite=TRUE)
#writeRaster(solar_radiation, "solar_radiation.tif")
#writeRaster(h_footprint2, "h_footprint2.tif")
#writeRaster(bio1, "bio1.tif")
#writeRaster(bio15, "bio15.tif")
#writeRaster(bio4, "bio4.tif")

################################################################################
# Check the NA values
################################################################################
#13-12-2023

#Load packages (to plot)
library(ggraph)
library(tidygraph)

#Divide the networks with zero overall distance (those producing NA in the distance components) 
#from the others
no_na_df <- final_data_frame_8[final_data_frame_8$sq_wasserstein_in_out_distance != 0,]
yes_na_df <- final_data_frame_8[final_data_frame_8$sq_wasserstein_in_out_distance == 0,]
#View the distances, just to check...
#names(no_na_df)
#names(yes_na_df)
#
View(no_na_df[,c(40:43)])
View(yes_na_df[,c(40:43)])
#
#no_na_df$nnodes
#yes_na_df$nnodes
#
#nrow(final_data_frame_8)
#nrow(no_na_df)
#nrow(yes_na_df)
#
#How many networks with zero distance
nrow(no_na_df)#how many with distance different from zero
nrow(yes_na_df)#how many with with distance zero
#
#Numbers of each dataset
no_na_df$network_number
yes_na_df$network_number
#
#And now... the in- and out-degrees
length(in_degree_list)
length(out_degree_list)

names_mut <- c()
names_ant <- c()
for(i in 1:length(mutualistic_networks)) names_mut[i] <- paste0("Network #", mutualistic_networks[[i]]$network$network_id)
for(i in 1:length(antagonistic_networks)) names_ant[i] <- paste0("Network #", antagonistic_networks[[i]]$network$network_id)

#How are the in degree distributions with NA (an example)
#in_degree_list[names(in_degree_list) %in% yes_na_df$network_number][55]
#How are the out degree distributions with NA (an example)
#out_degree_list[names(out_degree_list) %in% yes_na_df$network_number][55]

yes_na_df$network_number[70]

which(names_mut == yes_na_df$network_number[70])
which(names_ant == yes_na_df$network_number[70])

#Check the degree distribution
which(names(in_degree_list) == yes_na_df$network_number[70])
which(names(out_degree_list) == yes_na_df$network_number[70])
#
in_degree_list[[186]]
out_degree_list[[186]]

#Where is the network in the lines 34 an 36
#"Network #5184" %in% names_mut
#"Network #5184" %in% names_ant
#which("Network #5184" == names_mut)
#which("Network #5184" == names_ant)

##IF MUTUALISTIC

#To check the number of interactions between nodes
igraph::plot.igraph(mutualistic_networks_igraph[[186]])

#Now, knowing if ir is ANT our MUT, we see the network in that number
ggraph(as_tbl_graph(mutualistic_networks[[186]])) +
  geom_edge_link(arrow = arrow()) +
  geom_node_point() +
  theme_graph()

nrow(mutualistic_networks[[186]]$interactions[,2:3])
nrow(unique(mutualistic_networks[[186]]$interactions[,2:3]))

##IF ANTAGONIST

#To check the number of interactions between nodes
igraph::plot.igraph(antagonistic_networks_igraph[[222]])

#Now, knowing if ir is ANT our MUT, we see the network in that number
ggraph(as_tbl_graph(antagonistic_networks[[222]])) +
  geom_edge_link(arrow = arrow()) +
  geom_node_point() +
  theme_graph()

nrow(antagonistic_networks[[222]]$interactions[,2:3])
nrow(unique(antagonistic_networks[[222]]$interactions[,2:3]))


################################################################################
#
################################################################################



#Fit loess function to raw data
f_in <- loess(in_degree_list_selected_DF[[105]]$relative_freq ~ in_degree_list_selected_DF[[105]]$nr_vertices)
f_out <- loess(out_degree_list_selected_DF[[105]]$relative_freq ~ out_degree_list_selected_DF[[105]]$nr_vertices)

#Fidth order polynomials
f_in2 <- lm(in_degree_list_selected_DF[[105]]$relative_freq ~ poly(in_degree_list_selected_DF[[105]]$nr_vertices, 5, raw=TRUE))
f_out2 <- lm(out_degree_list_selected_DF[[105]]$relative_freq ~ poly(out_degree_list_selected_DF[[105]]$nr_vertices, 5, raw=TRUE))

#Plot loess fits
plot(predict(f_in), type = "l", col = "green", lwd = 2)
lines(predict(f_out), type = "l", col = "red", lwd = 2)

#Plot fifth order polynomials fits
plot(predict(f_in2), type = "l", col = "green", lwd = 2)
lines(predict(f_out2), type = "l", col = "red", lwd = 2)

#Plot raw and fits in in-degree
plot(in_degree_list_selected_DF[[105]], type = "l", col = "green", lwd = 2)
lines(predict(f_in), col='darkgreen', lwd=2)
lines(predict(f_in2), col='black', lwd=2)

#Plot raw and fits in out-degree
plot(out_degree_list_selected_DF[[105]], type = "l", col = "red", lwd = 2)
lines(predict(f_out), col='darkred', lwd=2)
lines(predict(f_out2), col='black', lwd=2)


################################################################################
#
################################################################################

plot(bio15)
plot(bio1 < 107)
plot(bio4 > 1.141e+4)
plot(solar_radiation < 1.124e+4)

plot(bio12 > 810.8)
plot(solar_radiation > 1.423e+4)


plot(h_footprint2 > 21)
plot(world, add=TRUE)
plot(final_data_frame_8_SPATIAL, add=TRUE)

#plot
plot(final_data_frame_8_SPATIAL[final_data_frame_8_SPATIAL$wildareas.v3.2009.human.footprint>21 & final_data_frame_8_SPATIAL$bio1<96,], add=TRUE)
#Which dataset?
final_data_frame_8_SPATIAL[final_data_frame_8_SPATIAL$wildareas.v3.2009.human.footprint>21 & final_data_frame_8_SPATIAL$bio1<96,]$dataset_id
#Network numbers?
final_data_frame_8_SPATIAL[final_data_frame_8_SPATIAL$wildareas.v3.2009.human.footprint>21 & final_data_frame_8_SPATIAL$bio1<96,]$network_number


################################################################################
# SC 5
################################################################################

################################################################################
# Tukey Honest Significant Differences
################################################################################

final_data_frame_9_ANT_2 <- as.data.frame(final_data_frame_9_ANT)
final_data_frame_9_MUT_2 <- as.data.frame(final_data_frame_9_MUT)

###
names(final_data_frame_9_ANT_2)
final_data_frame_9_ANT_2 <- final_data_frame_9_ANT_2[,39:41]
names(final_data_frame_9_ANT_2)
summary(final_data_frame_9_ANT_2)

df_loc <- data.frame(final_data_frame_9_ANT_2$sq_wasserstein_in_out_location_PERC, rep("location", nrow(final_data_frame_9_ANT_2)))
df_shape <- data.frame(final_data_frame_9_ANT_2$sq_wasserstein_in_out_shape_PERC, rep("shape", nrow(final_data_frame_9_ANT_2)))
df_size <- data.frame(final_data_frame_9_ANT_2$sq_wasserstein_in_out_size_PERC, rep("size", nrow(final_data_frame_9_ANT_2)))

names(df_loc) <- c("metric", "factor")
names(df_shape) <- c("metric", "factor")
names(df_size) <- c("metric", "factor")

final_data_frame_9_ANT_3 <- as.data.frame(rbind(df_loc, df_shape, df_size))
final_data_frame_9_ANT_3$metric <- as.numeric(final_data_frame_9_ANT_3$metric)
final_data_frame_9_ANT_3$factor <- as.factor(final_data_frame_9_ANT_3$factor)
#str(final_data_frame_9_ANT_3)

###

final_data_frame_9_MUT_2 <- final_data_frame_9_MUT_2[,39:41]
names(final_data_frame_9_MUT_2)
summary(final_data_frame_9_MUT_2)

df_loc2 <- data.frame(final_data_frame_9_MUT_2$sq_wasserstein_in_out_location_PERC, rep("location", nrow(final_data_frame_9_MUT_2)))
df_shape2 <- data.frame(final_data_frame_9_MUT_2$sq_wasserstein_in_out_shape_PERC, rep("shape", nrow(final_data_frame_9_MUT_2)))
df_size2 <- data.frame(final_data_frame_9_MUT_2$sq_wasserstein_in_out_size_PERC, rep("size", nrow(final_data_frame_9_MUT_2)))

names(df_loc2) <- c("metric", "factor")
names(df_shape2) <- c("metric", "factor")
names(df_size2) <- c("metric", "factor")

final_data_frame_9_MUT_3 <- as.data.frame(rbind(df_loc2, df_shape2, df_size2))
final_data_frame_9_MUT_3$metric <- as.numeric(final_data_frame_9_MUT_3$metric)
final_data_frame_9_MUT_3$factor <- as.factor(final_data_frame_9_MUT_3$factor)
#str(final_data_frame_9_MUT_3)

#How many with NA?
nrow(final_data_frame_9_ANT_2)
nrow(final_data_frame_9_ANT_2[complete.cases(final_data_frame_9_ANT_2),])

nrow(final_data_frame_9_MUT_2)
nrow(final_data_frame_9_MUT_2[complete.cases(final_data_frame_9_MUT_2),])

#Compute Tukey Honest Significant Differences
#Function from:
#https://rpubs.com/brouwern/plotTukeyHSD2

plotTukeysHSD <- function(tukey.out,
                          x.axis.label = "Comparison",
                          y.axis.label = "Effect Size",
                          axis.adjust = 0,
                          adjust.x.spacing = 5){
  
  tukey.out <- as.data.frame(tukey.out[[1]])
  means <- tukey.out$diff
  categories <- row.names(tukey.out)
  groups <- length(categories)
  ci.low <- tukey.out$lwr
  ci.up  <- tukey.out$upr                         
  
  n.means <- length(means)
  
  #determine where to plot points along x-axis
  x.values <- 1:n.means
  x.values <- x.values/adjust.x.spacing
  
  
  # calculate values for plotting limits            
  y.max <- max(ci.up) +                    
    max(ci.up)*axis.adjust
  y.min <- min(ci.low) - 
    max(ci.low)*axis.adjust
  
  if(groups == 2){ x.values <- c(0.25, 0.5)}
  if(groups == 3){ x.values <- c(0.25, 0.5,0.75)}
  
  x.axis.min <- min(x.values)-0.05
  x.axis.max <- max(x.values)+0.05
  
  x.limits <- c(x.axis.min,x.axis.max)
  
  #Plot means
  plot(means ~ x.values,
       xlim = x.limits,
       ylim = c(y.min,y.max),
       xaxt = "n",
       xlab = "",
       ylab = "",
       cex = 1.25,
       pch = 16)
  
  axis(side = 1, 
       at = x.values,
       labels = categories,
  )
  
  #Plot upper error bar 
  lwd. <- 2
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.up,
         x1 = x.values,
         length = 0,
         lwd = lwd.)
  
  #Plot lower error bar
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.low,
         x1 = x.values,
         length = 0,
         lwd = lwd.) 
  
  #add reference line at 0
  abline(h = 0, col = 2, lwd = 2, lty =2)
  
  mtext(text = x.axis.label,side = 1,line = 1.75)
  mtext(text = y.axis.label,side = 2,line = 1.95)
  mtext(text = "Error bars = 95% CI",side = 3,line = 0,adj = 0)
  
  
}


ANT.aov <- aov(metric ~ factor, 
               data = final_data_frame_9_ANT_3)
ANT.Tukey <- TukeyHSD(ANT.aov)

##

MUT.aov <- aov(metric ~ factor, 
               data = final_data_frame_9_MUT_3)
MUT.Tukey <- TukeyHSD(MUT.aov)

#Plot
par(mfrow = c(1, 2))
plotTukeysHSD(ANT.Tukey)
plotTukeysHSD(MUT.Tukey)

#Load - Save
#save.image(file = "inout_15DEZ23.RData")
#load("inout_15DEZ23.RData")

################################################################################
# Violin plots
################################################################################
#18-12-2023

names(final_data_frame_9_ANT_3)
names(final_data_frame_9_MUT_3)

ant_viol <- ggplot(final_data_frame_9_ANT_3, aes(x=factor, y=metric, fill=factor)) +
  geom_violin(scale = "count", trim = F, adjust = 0.75) +
  geom_point() +
  ggtitle("Antagonistic Networks (n=128)") +
  xlab("Wasserstein distance components") + 
  ylab("")

mut_viol <- ggplot(final_data_frame_9_MUT_3, aes(x=factor, y=metric, fill=factor)) +
  geom_violin(scale = "count", trim = F, adjust = 0.75) +
  geom_point() +
  ggtitle("Mutualistic Networks (n=93)") +
  xlab("Wasserstein distance components") + 
  ylab("")

ggarrange(ant_viol, mut_viol, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

################################################################################
# Violin plots - overall distance
################################################################################
#20-12-2023

df1 <- data.frame(final_data_frame_9_ANT$distance, rep("antagonistic", length(final_data_frame_9_ANT$distance)))
df2 <- data.frame(final_data_frame_9_MUT$distance, rep("mutualistic", length(final_data_frame_9_MUT$distance)))

names(df1) <- c("distance", "type")
names(df2) <- c("distance", "type")

df3 <- rbind(df1, df2)

all_viol <- ggplot(df3, aes(x=type, y=distance, fill=type)) +
  geom_violin(scale = "count", trim = F, adjust = 0.75) +
  geom_point() +
  ggtitle("Distance in antagonistic and mutualistic networks") +
  xlab("Network types") + 
  ylab("") +
  theme(legend.position = "none")

all_viol

################################################################################
# SC 6
################################################################################

################################################################################
# References
################################################################################

#Library
library(readxl)
library(stringr)

refs1 <- readxl::read_excel(
  "refs.xlsx",
  sheet = 1
)

refs1 <- as.data.frame(refs1)
View(refs1)

retained_id <- c()

for(i in 1:nrow(final_data_frame_8_SPATIAL)) retained_id[i] <- stringr::str_split(final_data_frame_8_SPATIAL$network_number[i], " #")[[1]][2]

#names(refs1)
refs2 <- data.frame(matrix(ncol = ncol(refs1)))
names(refs2) <- names(refs1)

for(i in 1:nrow(refs1)){
  
  row1 <- refs1[i,]
  
  if(stringr::str_count(refs1[i,]$`Network Id`, ",")!=0){
    
    id_row <- stringr::str_split(refs1[i,]$`Network Id`, ", ")[[1]]
    
  }else id_row <- refs1[i,]$`Network Id`
  
  if(any(id_row %in% retained_id)) refs2 <- rbind(refs2, row1)
  
}
nrow(refs2)

write.csv(refs2, file = "refs2.csv")


################################################################################
# SC 7
################################################################################

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

################################################################################
# SC8
################################################################################

################################################################################
# Area between the two curves
################################################################################
#FMestre
#19-12-2023

#library(tab)

names(in_degree_list)
names(out_degree_list)

in_degree_list_selected <- in_degree_list[names(in_degree_list) %in% final_data_frame_8_SPATIAL$network_number]
out_degree_list_selected <- out_degree_list[names(out_degree_list) %in% final_data_frame_8_SPATIAL$network_number]

#length(in_degree_list_selected)
#length(out_degree_list_selected)
#
#names(in_degree_list_selected)
#names(out_degree_list_selected)

#plot(mutualistic_networks_igraph[[110]])
#degree_distribution(mutualistic_networks_igraph[[110]], cumulative = TRUE, mode = "in")
#degree_distribution(mutualistic_networks_igraph[[110]], cumulative = TRUE, mode = "out")
#dev.off()

in_degree_list_selected_DF <- list()
out_degree_list_selected_DF <- list()

for(i in 1:length(in_degree_list_selected)){
  
  df_in <- data.frame(0:(length(in_degree_list_selected[[i]])-1), in_degree_list_selected[[i]])
  df_out <- data.frame(0:(length(out_degree_list_selected[[i]])-1), out_degree_list_selected[[i]])
  
  names(df_in) <- c("nr_vertices", "relative_freq")
  names(df_out) <- c("nr_vertices", "relative_freq")
  
  in_degree_list_selected_DF[[i]] <- df_in
  out_degree_list_selected_DF[[i]] <- df_out
  
}

names(in_degree_list_selected_DF) <- names(out_degree_list_selected)
names(out_degree_list_selected_DF) <- names(out_degree_list_selected)

dif_in_out <- c()

for(i in 1:length(in_degree_list_selected_DF)){
  
  #Use this as an example
  #plot(in_degree_list_selected_DF[[i]], type = "l", col = "green", lwd = 2)
  #lines(out_degree_list_selected_DF[[i]], type = "l", col = "red", lwd = 2)
  
  
  in_out_lines <- merge(in_degree_list_selected_DF[[i]],
                        out_degree_list_selected_DF[[i]],
                        by = "nr_vertices", 
                        all=TRUE
  )
  
  names(in_out_lines) <- c("degree", "in_freq", "out_freq")
  
  dif_in_out[i] <- mean(in_out_lines$in_freq - in_out_lines$out_freq, na.rm = TRUE)
  
}

dif_in_out_DF <- data.frame(names(out_degree_list_selected), dif_in_out)

names(dif_in_out_DF) <- c("network_id", "average_difference")
head(dif_in_out_DF)


df_variables_difference <- data.frame(final_data_frame_8_SPATIAL)
df_variables_difference <- data.frame(df_variables_difference, dif_in_out_DF)
df_variables_difference <- df_variables_difference[,-c(33,35, 42)]
names(df_variables_difference)[33] <- "solar_radiation" 
names(df_variables_difference)[34] <- "human_footprint" 

df_variables_difference_MUT <- df_variables_difference[df_variables_difference$type == "mutualistic",]
df_variables_difference_ANT <- df_variables_difference[df_variables_difference$type == "antagonistic",]

glm_ANT <- glm(average_difference ~ ., data = df_variables_difference_ANT[,-c(1:13, 35:39)])
glm_MUT <- glm(average_difference ~ ., data = df_variables_difference_MUT[,-c(1:13, 35:39)])

summary(glm_ANT)
summary(glm_MUT)

#?tabglm

#tab::tabglm(glm_ANT, xvarlabels = list( bio1 = "Annual Mean Temperature", 
#                                        bio9 = "Mean Temperature of DriestQuarter", 
#                                        bio12 = "Annual Mean Specific Humidity", 
#                                        bio14 = "Precipitation of Driest Month", 
#                                        solar_radiation = "Solar radiation", 
#                                        human_footprint = "Human footprint"
#                                       )
#            )







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

#From: https://github.com/araastat/reprtree/blob/master/R/ReprTree.R
#(installed the package reprtree instead)
ReprTree <- function(rforest, newdata, metric='d2'){
  
  # rforest A randomForest object
  # newdata The data on which predictions will be computed
  # metric The metric to be used to evaluate distance between trees. Currently
  # only the d2 metric is implemented
  #A list object containing representations of the representative trees
  #conformable with the \code{tree} class. Names of the list give the indices
  #of the representative trees in the set of trees. 
  
  if(metric!='d2') stop('invalid metric!')
  require(randomForest)
  print('Constructing distance matrix...')
  preds <- predict2(rforest, newdata=newdata, predict.all=T)
  preds.indiv <- preds$individual
  d <- dist.fn(t(preds.indiv), method=ifelse(rforest$type=='classification',
                                             'mismatch',
                                             'euclidean'))
  print('Finding representative trees...')
  D <- colMeans(d)
  index <- which(D==min(D))
  trees <- lapply(as.list(index), function(i) getTree(rforest, i, labelVar=TRUE))
  names(trees) <- as.character(index)
  trees <- lapply(trees, as.tree, rforest)
  out <- list(trees=trees,D = D)
  class(out) <- c('reprtree','list')
  return(out)
}

#### FUNCTIONS TO PLOT TREES - END - NOT USED



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
#                                 MANOVA
################################################################################

#?manova 

ANT_manova <- manova(cbind(final_data_frame_9_ANT[,40], final_data_frame_9_ANT[,41], final_data_frame_9_ANT[,42]) ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
                     data = final_data_frame_9_ANT)

summary(ANT_manova, test="Pillai")

##

MUT_manova <- manova(cbind(final_data_frame_9_MUT[,39], final_data_frame_9_MUT[,40], final_data_frame_9_MUT[,41]) ~ bio1+bio4+bio12+bio15+solar_radiation+human_footprint,
                     data = final_data_frame_9_MUT)

summary(MUT_manova, test="Pillai")


################################################################################
#                                      GAM
################################################################################

#FMestre
#05-06-2024
#Useful video: https://www.youtube.com/watch?v=sgw4cu8hrZM&t=13s
#citation("mgcv")

#final_data_frame_9_ANT
#final_data_frame_9_MUT

#Getting information on lat long
final_data_frame_9_shp <- terra::vect("C:/Users/asus/Documents/0. Artigos/4. SUBMETIDOS/in_out_degree/shapes/final_data_frame_9.shp")
final_data_frame_9_shp <- as.data.frame(final_data_frame_9_shp)

final_data_frame_9_ANT_v2 <- data.frame(final_data_frame_9_ANT, NA, NA)
names(final_data_frame_9_ANT_v2)[43:44] <- c("lat", "long")

for(i in 1:nrow(final_data_frame_9_ANT_v2)){
  
  row_ANT <- final_data_frame_9_ANT_v2[i,]
  row_ANT_network_number <- row_ANT$network_number
  row_ANT_dataset_id <- row_ANT$dataset_id
  df0 <- final_data_frame_8_shp[final_data_frame_8_shp$network_n0 == row_ANT_network_number & final_data_frame_8_shp$dataset_id == row_ANT_dataset_id,]
  final_data_frame_9_ANT_v2$lat[i] <- df0$lat
  final_data_frame_9_ANT_v2$long[i] <- df0$long
  
}

##

final_data_frame_9_MUT_v2 <- data.frame(final_data_frame_9_MUT, NA, NA)
names(final_data_frame_9_MUT_v2)[42:43] <- c("lat", "long")

for(i in 1:nrow(final_data_frame_9_MUT_v2)){
  
  row_MUT <- final_data_frame_9_MUT_v2[i,]
  row_MUT_network_number <- row_MUT$network_number
  row_MUT_dataset_id <- row_MUT$dataset_id
  df1 <- final_data_frame_8_shp[final_data_frame_8_shp$network_n0 == row_MUT_network_number & final_data_frame_8_shp$dataset_id == row_MUT_dataset_id,]
  final_data_frame_9_MUT_v2$lat[i] <- df1$lat
  final_data_frame_9_MUT_v2$long[i] <- df1$long
  
}


#?mgcv::gam

gam_fw <- mgcv::gam(distance ~ s(bio12, k=10)+s(bio15, k=15)+s(solar_radiation, k=10)+s(human_footprint, k=8), correlation=corGaus(1,form=~lat+long),
                    data= final_data_frame_9_ANT_v2,
                    family = gaussian,
                    methods = "REML"
)

summary(gam_fw)
gam.check(gam_fw)
as.vector(predict(gam_fw, final_data_frame_9_ANT_v2))
mgcv::plot.gam(gam_fw)
gratia::draw(gam_fw, scales = "fixed")

#####

gam_mut <- mgcv::gam(distance ~ s(bio12, k=4)+s(bio15, k=4)+s(solar_radiation, k=4)+s(human_footprint, k=4), correlation=corGaus(1,form=~lat+long),
                     data= final_data_frame_9_MUT_v2,
                     family = gaussian,
                     methods = "REML"
)

summary(gam_mut)
gam.check(gam_mut)
as.vector(predict(gam_mut, final_data_frame_9_MUT_v2))
mgcv::plot.gam(gam_mut)
gratia::draw(gam_mut, scales = "fixed")

#plot(gam_mut, pages = 1, all.terms = TRUE, rug = TRUE, residuals = TRUE, 
#pch = 1, cex = 1, shade = TRUE, seWithMean = TRUE, shift = coef(gam_mut)[1])

#ggplot(data = final_data_frame_9_MUT_v2, aes(y = distance, x = human_footprint)) +
#  geom_point() + 
#  theme_bw() +
#  geom_line(aes(x = human_footprint, y = fitted(gam_mut)), colour = "blue", linewidth = 1.2)


#################

#predict_fw <- predict(gam_fw, newdata = final_data_frame_9_ANT_v2, type = "response", se.fit = TRUE)

# Plot the data and the GAM fit
#ggplot() +
#  geom_point(data = final_data_frame_9_ANT_v2, aes(x = human_footprint, y = distance)) +
#  geom_line(data = data.frame(hf = final_data_frame_9_ANT_v2$human_footprint, fit1 = predict_fw$fit), 
#            aes(x = hf, y = fit1), color = "blue", size = 1) +
#  geom_ribbon(data = data.frame(hf = final_data_frame_9_ANT_v2$human_footprint, fit1 = predict_fw$fit, se = predict_fw$se.fit), 
#            aes(x = hf, ymin = fit1 - 1.96 * se, 
#            ymax = fit1 + 1.96 * se), alpha = 0.3) +
#  
#  labs(title = "Generalized Additive Model (GAM)", 
#       x = "Human Footprint", y = "Distance") +
#  theme_minimal()



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
#red_fw <- predict(bagged_cv_FW, final_data_frame_10_ANT_test)
#RMSE(red_fw, final_data_frame_10_ANT_test[complete.cases(final_data_frame_10_ANT_test),]$distance)


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
#red_mut <- predict(bagged_cv_MUT, final_data_frame_10_MUT_test)
#RMSE(red_mut, final_data_frame_10_MUT_test[complete.cases(final_data_frame_10_MUT_test),]$distance)





