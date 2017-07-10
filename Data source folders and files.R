###DATA WORKING DIRECTORY AND SOURCE FILE

#clear all previous data from R recall memory
rm(list=ls())

#get data

#link location of BIO4158 data files to R (set working directory)

  if (getwd()!="C:/Users/Eric/Documents/BIO4158_Lab_data_files") {
      setwd("C:/Users/Eric/Documents/BIO4158_Lab_data_files")
       }

  if (getwd()=="C:/Users/Eric/Documents/BIO4158_Lab_data_files"){
      print("SOURCE DATA FOLDER IS LINKED TO 'BIO4158_Lab_data_files' ON ERIC'S PC")
      } else 
        {print("INDICATED WORKING DIRECTORY IS NOT AVAILABLE")
        }


#load sturgeon data from previously identified working directory

load(file="sturgeon.Rdata")

#data check for "sturgeon" data frame
ls(sturgeon)
ls.str(sturgeon)
summary(sturgeon)

#make headers from data frame "sturgeon" directly callable in R
attach(sturgeon)
