################################################################################
# Tukey Honest Significant Differences
################################################################################
#12-12-2023

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

