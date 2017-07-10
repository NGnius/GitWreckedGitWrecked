#BIND MATRICES

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
#Combine two matrices (of same # of cols horizontally)####

  #import matrices to bind
  matrix1 <- as.matrix(read.csv(file = "matrix_bind_1.csv", header = FALSE))
  
  matrix2 <- as.matrix(read.csv(file = "matrix_bind_2.csv", header = FALSE)) 
  
    #verify import
    matrix1
    matrix2
    
#BIND ROWS
    
    #new object
    matrices_binded_rows <- rbind(matrix1,matrix2)
    
    #verify row bind
    
    matrices_binded_rows
    
#BIND COLUMNS
    
    #new object
    matrices_binded_cols <- cbind(matrix1, matrix2)
    
    #verify column bind
    matrices_binded_cols
    
    
#ADD TEXT TO MATRIX (IE. A FACTOR COLUMN)
   
    #create single dataframe column (aka vector analogue) by repeating the term 
      #"vector" for 10 iterations 
    test_vector <- data.frame(rep("vector", 10), stringsAsFactors = TRUE) 
    colnames(test_vector) <- c("type")
   
   #combine matrices with factor levels
   cbind(matrices_binded_cols, test_vector)
   