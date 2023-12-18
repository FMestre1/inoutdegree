################################################################################
# References
################################################################################
#18-12-2023

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

