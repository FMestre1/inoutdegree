###########################################################################
#TEST OTHER DATABASES

library(RCurl)
library(XML)
library(plyr)

#EcoBase
eb1 <- create.fw.list(db ="eb", ecosyst = TRUE, spatial = TRUE, ref = TRUE)
eb_list2_non_numeric <- remove.non.numeric(eb1)

#GlobalWeb
gw1 <- create.fw.list(db ="gw", folder="globalweb", ecosyst = TRUE, ref = TRUE, code = TRUE)
gw1$int_matrix
View(gw1$int_matrix[[1]])
gw1$ecosystem
View(gw1$references)
gw1$code

gw2 <- remove.repeated.names(gw1)
gw3 <- remove.non.numeric(gw2)
###########################################################################