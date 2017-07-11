#CURVE DATA EXPORT TOOL

#INSTRUCTIONS####

#REQUIRED PACAKGES
#If the packages indicated below are not installed, install them by uncommenting
#and running the scripts (select text, then Ctrl+R). Re-comment the uncommented
#packages before running the rest of this script.

#RUN THE EXPORT SCRIPT
#Select all of the text in this script (Ctrl+A) and then clicking Ctrl+R to run.

#************************************************************************************
#REQUIRED PACKAGE INSTALLATION####

#tcltk message box package (should not need to install since this is a base pkg)
#install.packages("tcltk", dependencies=TRUE)

#others TBD

#**********************************************************************************
#SET-UP ####

#clear all previous data from R recall memory
rm(list=ls())

#************************************************************************************
#SET WORKING DIRECTORY AND SELECT SOURCE NAME CSV FILE ####

#Message box package
library(tcltk)

#--> find a way to use the tktoplevel() fxn to always bring the dialogue box to the
#top window w/out affecting evaluation of the expressions

#Initial warning message
msgBox_initial <- tktoplevel(tk_messageBox(title = "IMPORTANT INFORMATION", 
                                           message = "IMPORTANT: Message boxes will not always appear on the top level (ie. above the R or RStudio screen). If script continues running but does not produce any output, check lower levels for dialogue boxes."))

#+Set working directory (optional) ####

msgBox_wd1 <- tk_messageBox(title = "Set working directory",
                            message = "Do you want to set the location of the stereomorph curve and landmark data output (.txt) files? Click 'No' if it has already been set.", 
                            type = "yesno")

#Begin repeat loop
if(msgBox_wd1 == "yes") {
  
  repeat {
    #choose working directory location
    setwd(choose.dir(caption = "Choose the location of the working directory containing the curve and landmark data source CSV file and .txt stereomorph outputs"))
    #Display working directory location
    msgBox_wd_location <- tk_messageBox(title = "Set working directory",
                                        message = paste(c("Working directory set to: ", c(getwd()), 
                                                          "

Click 'Yes' to accept this directory or 'No' to retry"),
                                                        sep = "", collapse = ""), 
                                        type = "yesno")
    if(msgBox_wd_location == "yes") {
      break
    } 
    else {
      msgBox_wd_locationFAIL <- tk_messageBox(title = "Set working directory", 
                                              message = "Do you want to retry to set the working directory?", 
                                              type = "retrycancel")
      if(msgBox_wd_locationFAIL == "cancel") {
        break
      } else {}
      
    }
  }
  
} else {
  msgBox_wd2 <- tk_messageBox(title = "Set working directory", 
                              message = "No new working directory has been set. Proceeding to source file selection.", 
                              type = "ok", icon = "warning")
} 


#***  
#+Set shape names source file (mandatory) ####
msgBox_ShapeNamesSource <- tk_messageBox(title = "Set shape names source file", 
                                         message = "Choose the shape names source file.
 
The name source file must be a single column which contains only the same shape names used to name the pictures that were imported into the stereomorph landmark/curve digitization function in the .csv format. The correct format for the .csv file is presented below:

  <no column header>  <no other columns>
  shape_1_1
  shape_1_2
  shape_2_1
  shape_2_2
  etc.
", type = "ok")

ShapeNamesSource <- read.table(choose.files(caption = "Select the shape names .csv file"), 
                               sep = "", header = FALSE)


#***
#+Select shape name in GUI (WORKS...not pretty tho!)####
  #this will import the corresponding .txt file from the working directory

#Convert ShapeNamesSource to character vector to be used by the GUI
  ShapeNamesSource_vector <- as.vector(ShapeNamesSource[,1], mode = "any")


#Shape selection repeat loop

  #Begin repeat loop
  repeat {
    #selection pane
    listBox_choice <- tk_select.list(title = "Select the StereoMorph shape data file to import", 
                                     ShapeNamesSource_vector, multiple = FALSE)
    
    #selection confirmation
    listBox_SelectionConfirm <- tk_messageBox(title = "Confirm shape data file selection", 
                                              message = paste0(listBox_choice," is selected as the shape data file. Use this file?",
                                                               sep = ""), 
                                              type = "yesno")
    if(listBox_SelectionConfirm == "yes") {
      break} 
    else {}
  }

#Confirm file choice in console 
  paste0("Curves list will be generated for ",listBox_choice,
         " StereoMorph output file.", sep = "")


#***********************************************************************************
#LOAD SHAPES DATA FOR ONE StereoMorph OUTPUT FILE AT A TIME ####
#name was selected above in via the selection GUI

library(StereoMorph)

#select data output   

  #first create object w/ .txt filetype extension out of the listBox_choice
  listBox_choiceTXT <- paste(listBox_choice, ".txt", sep = "")
  
  #Create a generic object that will be processed by the loop
  ShapeDataOutput <- readShapes(paste0(listBox_choiceTXT))


#***********************************************************************************
#EXTRACT SCALED CURVE DATA AND WRITE TO CSV ####

#+Set curve name source files (mandatory)####
msgBox_CurveNamesSource <- tk_messageBox(title = "Set curve names source file", 
                                            message = "Choose the curve names source file.

Select the same .txt file used to name the curves in the StereoMorph curve digitization function.", 
                                            type = "ok")
                                            
CurveNamesSource <- read.delim(choose.files(caption = "Select the source curve names .txt file"), 
                                  sep = "\t", header = FALSE, 
                               stringsAsFactors = FALSE)

#***
#CREATE EMPTY LIST TO DUMP ALL SCALED CURVE DATA INTO####
  #(keep outside of loop)

All_Curves_SCALED_list <- list()

#***  
#BEGIN FOR-LOOP FOR ALL CURVE NAMES IN [,1] of CurveNamesSource df####

for(i in 1:c(nrow(CurveNamesSource))) {
  

  #+Isolate curve data####
  CURVES_ShapeDataOutput <- as.data.frame(ShapeDataOutput$curves.scaled[[i]])
  
  
  #rename the two columns of the CURVES_ShapeDataOutput df
  colnames(CURVES_ShapeDataOutput) <- c("X","Y")


#***  
#+Create additional factor levels to add to the XY coord data####  

  #+---shape source####
  #Create shape source factor column dataframe
  SHAPE_FACTOR  <- data.frame(rep(listBox_choice,
                                  nrow(ShapeDataOutput$curves.scaled[[i]])),
                              stringsAsFactors = TRUE)

    #rename the text vector column
    colnames(SHAPE_FACTOR) <- c("shape_name")

    
  #+---curve####
  #Create curve factor column dataframe
  CURVE_FACTOR  <- data.frame(rep("curve", 
                                     nrow(ShapeDataOutput$curves.scaled[[i]])),
                                 stringsAsFactors = TRUE)

    #rename the text vector column
    colnames(CURVE_FACTOR) <- c("point_type")
  
  
  #+---curve name####
  #Create curve factor column dataframe
  CURVENAME_FACTOR  <- data.frame(rep(CurveNamesSource[i,1], 
                                  nrow(ShapeDataOutput$curves.scaled[[i]])),
                              stringsAsFactors = TRUE)
  
    #rename the text vector column
    colnames(CURVENAME_FACTOR) <- c("curve_name")

  
  #+---shape scaling####
  #Create shape scaling factor column dataframe
  SCALING_FACTOR  <- data.frame(rep(ShapeDataOutput$scaling, 
                                    nrow(ShapeDataOutput$curves.scaled[[i]])),
                                stringsAsFactors = TRUE)

    #rename the text vector column
    colnames(SCALING_FACTOR) <- paste(ShapeDataOutput$scaling.units, "/pixel", sep = "")


#***  
#+Generate intermediate single curve dataframe####

#bind CURVES_ShapeDataOutput, SHAPE_FACTOR, CURVE_FACTOR, CURVENAME_FACTOR and 
  #SCALING_FACTOR
Curve_SCALED <- cbind(CURVES_ShapeDataOutput, SHAPE_FACTOR, CURVE_FACTOR, 
                       CURVENAME_FACTOR, SCALING_FACTOR)


#***
#+Write intermediate curve dataframe to All_Curves_SCALED_list as a list####

  #convert Curve_SCALED loop variable to list value and add it to list entry 'i' 
    #of All_Curves_SCALED_list 
  All_Curves_SCALED_list[[i]] <- as.list(Curve_SCALED)
}

#***
#+Success message####

msgBox_success <- tk_messageBox(title = "Sucess!", 
                                message = paste0(paste0(CurveNamesSource[1:c(nrow(CurveNamesSource)-1),1], sep = " "
                                                  ),
                                                 "and ", 
                                                 CurveNamesSource[nrow(CurveNamesSource),1],
                                                 "

from the shape ", 
                                                   listBox_choice,
                                                 " were successfully written to .csv files in the folder

",
                                                 getwd(), sep = ""), 
                                type = "ok")

#Confirmation in console
  paste0("SUCCESS! ", paste0(CurveNamesSource[1:c(nrow(CurveNamesSource)-1),1], sep = " "),
  "and ", CurveNamesSource[nrow(CurveNamesSource),1]," from the shape ", 
  listBox_choice, " were successfully written to .csv files in the folder ",
  getwd(), sep = "")




#********************************************************************************
#ROW BIND ALL CURVE SEGMENT DATAFRAMES TO CREATE FINAL CURVE MATRIX####

library(data.table)

  #Row bind all sublists in the All_Curves_SCALED_list by column names
  All_Curves_SCALED <- rbindlist(All_Curves_SCALED_list,use.names = TRUE)
  
  #+Write All_Curves_SCALED dataframe to .csv file####
  write.csv(All_Curves_SCALED, file = paste0("All_Curves_SCALED_", listBox_choice, 
                                              ".csv", sep = "")
            )


#Confirm intermediate curve df generation in console 
  paste0("A scaled curve dataframe was generated and written to .csv file for all curves of", 
         listBox_choice, sep = "")
