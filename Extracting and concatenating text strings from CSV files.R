#EXTRACTING AND CONCATENATING TEXT STRINGS FROM CSV FILES

#**********************************************************************************
#SET-UP####


#clear all previous data from R recall memory
rm(list=ls())

#get data

#link location of morphometrics data to R (set working directory)

if (getwd()!="C:/Users/Eric/Documents/Eric/School/University/Fall 2016-Winter 2017/BIO 4158/Labs/R labs/Tools") {
  setwd("C:/Users/Eric/Documents/Eric/School/University/Fall 2016-Winter 2017/BIO 4158/Labs/R labs/Tools")
}

#DATA SOURCE CHECK

if (getwd()=="C:/Users/Eric/Documents/Eric/School/University/Fall 2016-Winter 2017/BIO 4158/Labs/R labs/Tools"){
  print("SOURCE DATA FOLDER IS LINKED TO 'Tools' FOLDER ON ERIC'S PC")
} else 
{print("INDICATED WORKING DIRECTORY IS NOT AVAILABLE")
}

#*********************************************************************************
#Import txt file####

words_3col <- as.data.frame(read.table(file = "random_words_3col.txt", header = FALSE, 
                            sep = ""))
#rename columns
colnames(words_3col) <- c("curve_name", "start_pt", "end_pt")

#*********************************************************************************
#Concatenate all rows into one column####
  #create new vector to dump these new strings into
  string_words_3cols <- as.data.frame(vector(mode = "character", length = 4), 
                                      stringsAsFactors = FALSE #this means all 
                                      #text strings will instead be treated as 
                                      #characters only instead of factor levels
                                      )
  
  #concatenating loop
  for(i in 1:c(nrow(words_3col))) {
    string_words_3cols[i,1] <- paste(words_3col[i,1], words_3col[i,2], 
                                     words_3col[i,3], sep = "_")
  }
  
    #verify 
    string_words_3cols
  