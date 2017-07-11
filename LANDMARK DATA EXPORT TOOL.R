#LANDMARK DATA EXPORT TOOL

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
      
  #--> generate a selection pane from the ShapeNamesSource list
    
    #Convert ShapeNamesSource to character vector to be used by the GUI
    ShapeNamesSource_vector <- as.vector(ShapeNamesSource[,1], mode = "any")
    
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
    paste0("Landmark list will be generated for ",listBox_choice,
            " StereoMorph output file.", sep = "")
      
      
#***********************************************************************************
#LOAD SHAPES DATA FOR ONE StereoMorph OUTPUT FILE AT A TIME ####
    #name was selected above in via the selection GUI
  
  library(StereoMorph)
  
  #select data output   
    
    #first create object w/ .txt filetype extension out of the listBox_choice
    listBox_choiceTXT <- paste(listBox_choice, ".txt", sep = "")
    
  ShapeDataOutput <- readShapes(paste0(listBox_choiceTXT))
  
#***********************************************************************************
#EXTRACT SCALED LANDMARK DATA AND WRITE TO CSV ####
  
    
    
    #+Set landmark name source files (mandatory)####
    msgBox_LandmarkNamesSource <- tk_messageBox(title = "Set landmark name source file", 
                                                message = "Choose the landmark name source file.

Select the same .txt file used to name the landmarks in the StereoMorph landmark digitization function.", 
                                                type = "ok")
    LandmarkNamesSource <- read.table(choose.files(caption = "Select the source landmark names .txt file"), 
                                      sep = "", header = FALSE)
    
    
#***  
  #+Isolate landmark data####
  LANDMARKS_ShapeDataOutput <- as.data.frame(ShapeDataOutput$landmarks.scaled)
    
    
  #rename the two columns of the LANDMARKS_ShapeDataOutput df
  colnames(LANDMARKS_ShapeDataOutput) <- c("X","Y")
  
  
#***  
  #+Create additional factor levels to add to the XY coord data####  
  
  #---shape factor####
  #Create shape source factor column dataframe
  SHAPE_FACTOR  <- data.frame(rep(listBox_choice, 
                                    nrow(LandmarkNamesSource)),
                                stringsAsFactors = TRUE)
  
    #rename the text vector column
    colnames(SHAPE_FACTOR) <- c("shape_name")
  
  #+---landmark factor####  
  #Create landmark factor column dataframe
  LANDMARK_FACTOR  <- data.frame(rep("landmark", 
                                    nrow(LandmarkNamesSource)),
                                stringsAsFactors = TRUE)
    
    #rename the text vector column
    colnames(LANDMARK_FACTOR) <- c("point_type")
  
  
  #+---curve name####
  #Create curve factor column dataframe
  CURVENAME_FACTOR  <- data.frame(rep(NA, 
                                      nrow(LandmarkNamesSource)),
                                  stringsAsFactors = TRUE)
  
    #rename the text vector column
    colnames(CURVENAME_FACTOR) <- c("curve_name")
    
  #+---scaling factor####  
  #Create shape scaling factor column dataframe
  SCALING_FACTOR  <- data.frame(rep(ShapeDataOutput$scaling, 
                                  nrow(LandmarkNamesSource)),
                              stringsAsFactors = TRUE)
    
    #rename the text vector column
    colnames(SCALING_FACTOR) <- paste(ShapeDataOutput$scaling.units, "/pixel", sep = "")
    
    
#***  
  #+Generate final landmarks dataframe####
  
  #bind LANDMARKS_ShapeDataOutput and SHAPE_FACTOR
  All_Landmarks_SCALED <- cbind(LANDMARKS_ShapeDataOutput, SHAPE_FACTOR, 
                                LANDMARK_FACTOR, SCALING_FACTOR)
    
  #Rename this dataframe to include the original shape name (in case there is a need to keep this dataframe in the R environment)
  assign(paste0("All_Landmarks_SCALED_", listBox_choice, sep = ""), 
         All_Landmarks_SCALED)


#***
  #+Write final landmarks dataframe to .csv file####
  write.csv(All_Landmarks_SCALED, file = paste0("All_Landmarks_SCALED_", 
                                                listBox_choice, ".csv", sep = "")
            )
    

#***
#+Success message####
  
  msgBox_success <- tk_messageBox(title = "Sucess!", 
                                  message = paste0("All_Landmarks_SCALED_",
                                                   listBox_choice,
                                                   ".csv was successfully written to ",
                                                   getwd(), sep = ""))
  
  paste0("Success! All_Landmarks_SCALED_",listBox_choice, ".csv was successfully written to ",
         getwd(), sep = "")
#********************************************************************************