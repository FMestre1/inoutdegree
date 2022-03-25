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






