library(readxl)




wtest <- read_excel("Data/2021_data.xls", na = "NA")


wtest$ID <- paste(wtest$ID, "_", as.numeric(format(wtest$Date,'%Y')), sep = "")

wtest$Date <- as.POSIXct(wtest$Date)

wtest$Date <- as.character(wtest$Date)

df <- data.frame(ID = NA, Species = NA, Date = NA, Region = NA, Site = NA, N_eggs = NA, W_test = NA)


for(i in 1:nrow(wtest)){
  dd <- as.data.frame(wtest[i, ])
  angl_string <- unlist(strsplit(dd$W_test_angle,split=',', fixed=TRUE))
  diam_string <- unlist(strsplit(dd$W_test_diametr,split=',', fixed=TRUE))
  
  if(sum(!is.na(angl_string)) != 0){
  angl_string <- paste(rep("A_", length(angl_string)), angl_string, sep ="") 
  angl_string <- gsub(" ", "", angl_string )
  }
  
  if(sum(!is.na(diam_string)) != 0){
  diam_string <- unlist(strsplit(dd$W_test_diametr,split=',', fixed=TRUE))
  diam_string <- paste(rep("D_", length(diam_string)), diam_string, sep ="") 
  diam_string <- gsub(" ", "", diam_string )
  }

  
  w_test_all <- c(angl_string, diam_string)
  
  if(sum(!is.na(w_test_all)) == 0) df <-  rbind(df, data.frame(dd[ ,1:6], W_test = NA))
  else{
    w_test_all <- w_test_all[!is.na(w_test_all)] 
    n <- length(w_test_all)
    for(j in  1:n) df <- rbind(df, data.frame(dd[ ,1:6], W_test = w_test_all[j]))
    
  }
  
  print(i)
}


table(df$Region)


write.csv(df, "2021_clean.csv", fileEncoding = "UTF-8")


