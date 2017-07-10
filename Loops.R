#LOOPS IN R

#*************************************************************************************
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
} else {
  print("INDICATED WORKING DIRECTORY IS NOT AVAILABLE")
}


#*************************************************************************************
#SIMPLE FOR-LOOP####

  #Create text vector and have the loop count the number of letters in each string
    #and dump the values into a new dataframe
  
  #vector 
  words <- as.data.frame(c("eric","sean", "littlewood", "Thursday", "morphometrics"))
  
  #establish new empty dataframe to dump values into (can be any length)
  
  char_count <- as.data.frame(vector(mode="numeric", length=0))
  
    #verify empty_df
    char_count
  
  #LOOP
    #dump the character count of the ith entry of the char vector into the ith 
      #entry of the char_count dataframe
    
  for(i in 1:c(nrow(words))) {
    char_count[i,1] <- nchar(as.character(words[i,1]))
  }
  
  #view char_count
  char_count
  
#***  
  
  #Create text vector and have the loop count the number of letters in each string
  #and dump the values into any new column of the same dataframe
  
  #vector 
  words <- as.data.frame(c("eric","sean", "littlewood", "Thursday", "morphometrics"))
  
  #LOOP
  #dump the character count of the ith entry of the char vector into the ith row
  #of a new column proceeding the current position of the last column (ie. so the
  #current last row is not overwritten)
  
  #col number for new last column *keep this outside of the loop!
  col_words <- c(ncol(words)+1)
  
  #loop
  for(i in 1:c(nrow(words))) {
    words[i,col_words] <- nchar(as.character(words[i,1]))
  }
  
  #rename all columns
  colnames(words) <- c("word", "char_count")
  
    #alternatively, just rename the df's last column (ie. the newly added col)
    #colnames(words)[c(ncol(words))] <- "char_count"
    
  
  #view new words df
  words
  
  c("col_",words[1,2])

#*********************************************************************************
#SIMPLE REPEAT LOOP####
  
  #Create simple looping message box which will loop the prompt until the loop is 
   #broken with the 'No' button
  library(tcltk)
  
  #create initial message box prompt
  msgBox_initial <- tk_messageBox(title = "Looping messages", 
                                  message = "Would you like to start a looped message?", 
                                  type = "yesno")
  if(msgBox_initial == "yes") {
    repeat {
      msgBox_loop <- tk_messageBox(title = "In-loop message", 
                                   message = "Would you like to continue the loop?", 
                                   type = "yesno")
      if(msgBox_loop == "no") {
        msgBox_loopBREAK <- tk_messageBox(title = "Loop terminated", 
                                          message = "Loop terminated", 
                                          type = "ok"); 
        break
        } else {}
    }
  }
  