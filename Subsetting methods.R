####DATAFRAME SETUP AND SUBSETTING METHODS

#clear all previous data from R recall memory
rm(list=ls())

#get data

#link location of BIO4158 data files to R

if (getwd()!="C:/Users/Eric/Documents/BIO4158_Lab_data_files") {
  setwd("C:/Users/Eric/Documents/BIO4158_Lab_data_files")
}

if (getwd()=="C:/Users/Eric/Documents/BIO4158_Lab_data_files"){
  print("SOURCE DATA FOLDER IS LINKED TO 'BIO4158_Lab_data_files' ON ERIC'S PC")
}else{print("INDICATED WORKING DIRECTORY IS NOT AVAILABLE")}



#load climate data

load(file="climate.Rdata")

#data check for "climate" data frame
ls(climate)
ls.str(climate)
summary(climate)

#make headers from data frame "climate" directly callable in R
attach(climate)

##################################################################################

#DATAFRAME SETUP (WHERE ALL SOURCE DATA ORIGINATES FROM THE SAME DATAFRAME -> 
                  #eliminates chance of ghost data)
  region.north <- subset(climate, subset=loc=="A",
                          select = -c(reg,loc,calc.1)) #negative in front of 'c'
                                                        #means remove selected columns
  
  #rename the last column (aka col that was just added) as "Alert"
  names(region.north)[c(ncol(region.north))]<-"Alert"

#ADDING SUBSEQUENT COLUMNS
  
  #SUBSEQUENT COLUMN 1
    #subset the Mould data set of the climate df
    Mould <- subset(climate, subset=loc=="M",
                     select = -c(reg,year,loc,calc.1)) #drop unnecessary columns from subset
  
    #isolate the target column from the subset and save as a column vector "Mould.target"
    Mould.target.col <- Mould$ave #'ave' after $ is target column of Mould subset
  
    #attach to "Mould.target" to region.north dataframe; new column is called "Mould"
    region.north$Mould <- Mould.target.col
  
  #SUBSEQUENT COLUMN 2
    #subset the Sachs data set of the climate df
    Sachs <- subset(climate, subset=loc=="S",
                     select = -c(reg,year,loc,calc.1)) #drop unnecessary columns from subset
  
    #isolate the target column from the subset and save as a column vector "Sachs.target"
    Sachs.target.col <- Sachs$ave
  
    #attach to "Sachs.target" to region.north dataframe; new column is called "Sachs"
    region.north$Sachs <- Sachs.target.col
  
    attach(region.north)
    
#ADD CALCULATED COLUMN
    #ex. Means of certain columns in one row
    region.north$mean <- rowMeans(region.north[,c(2:4)], 
                                    na.rm=FALSE)
  
##################################################################################

#Other subsetting techniques can be found on the link below:
    #http://www.ats.ucla.edu/stat/r/modules/subsetting.htm

