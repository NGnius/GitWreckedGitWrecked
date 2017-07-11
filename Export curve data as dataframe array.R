#GENERATE A DATAFRAME ARRAY BY IMPORTING AND PROCESSING SEPARATE DATAFRAMES 
  #USING LOOPS

#********************************************************************************
#SET-UP####

  #clear all previous data from R recall memory
  rm(list=ls())
  
  #Set working directory
  
  setwd("C:/Users/Eric/Desktop")

#********************************************************************************
#CREATE SAMPLE DATA####
  
  #create 2 sample dfs

    #Create matrix 1 from vectors
    X <- as.vector(rep(1:2,2), mode = "numeric")
    Y <- as.vector(rep(c(3,5),2), mode = "numeric")
    
    shape_name <- as.factor(rep("shape_A",4))
    
    point_type <- as.factor(rep("landmark",4))
    
  #matrix 1
    M1 <- cbind.data.frame(X,Y,shape_name,point_type)
    write.csv(M1, "M1.csv")
    
    #Create matrix 2 from vectors
    X <- as.vector(rep(c(8.5,6),3), mode = "numeric")
    Y <- as.vector(rep(c(15,9),3), mode = "numeric")
    
    shape_name <- as.factor(rep(c("shape_B", "shape_C"),c(4,2)))
    
    point_type <- as.factor(rep("landmark",6))
    
  #matrix 2
    M2 <- cbind.data.frame(X,Y,shape_name,point_type)
    write.csv(M2, file = "M2.csv")
    

#Create list of matrices to bind to each other

    matrices_list <- as.character(c("M1", "M2"))
    write.table(matrices_list, file = "matrices_list.txt")
    

#create name of variable object to be processed by the loop
  #(this would normally be chosen via the GUI)
listBox_choice <- "matrices_list"

#import this file
listBox_choiceTXT <- paste(listBox_choice, ".txt", sep = "")

generic_list <- read.table(paste(listBox_choiceTXT))

#********************************************************************************
#MATRIX PROCESSING####

#+Create empty list to dump dataframes (as lists) into####
All_Dataframes_list <- list()


#+Import, alter and rbind two matrices together using loops####
    
    for(i in 1:c(nrow(generic_list))) {
    #import the file to dataframe
    M_intermediate <- as.data.frame(read.csv(file = paste(generic_list[i,1], ".csv", sep = ""), 
                                     header = TRUE)
                                     )
    
    #add "origin" columns factor to the imported matrix
    ORIGIN_FACTOR  <- data.frame(rep(paste(generic_list[i,1], sep = ""),
                                    nrow(M_intermediate)),
                                stringsAsFactors = TRUE)
    
    #rename the text vector column
    colnames(ORIGIN_FACTOR) <- c("origin")
    
    #bind the new column vector
    M_intermediate_final <- cbind.data.frame(M_intermediate, ORIGIN_FACTOR)
    
    
    #***
    #+Add to list####
    All_Dataframes_list[[i]] <- as.list(M_intermediate_final)
    
    }

#********************************************************************************
#COMBINE ALL LIST ELEMENTS INTO ONE STACKED DATAFRAME####

library(data.table)

#+Row bind all sublists into one big dataframe####
All_Dataframes_final <- rbindlist(All_Dataframes_list, use.names = TRUE)

  #verify
  All_Dataframes_final
  
  is.data.frame(All_Dataframes_final)
  
#+write to .csv file
  
  write.csv(All_Dataframes_final, file = "All_Dataframes.csv")
#********************************************************************************
