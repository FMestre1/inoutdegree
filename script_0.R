#FMestre
#16-03-2022

library(FWebs)


#Getting data in ##########################################################

#MANGAL ###################################################################

mg1 <- create.fw.list(db="mg", ref=TRUE, spatial = TRUE, code =TRUE, 
                      mangal_types =c("mutualism", "predation", "herbivory", 
                                      "scavenger", "detritivore"))


names(mg1$int_matrix)
View(mg1$references)
mg1$spatial_info
names(mg1)
nrow(mg1$references)
length(mg1$int_matrix)
mg1$code

save(mg1, file="mangal_dataset.RData")

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





