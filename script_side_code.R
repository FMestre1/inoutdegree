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









