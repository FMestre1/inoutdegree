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

