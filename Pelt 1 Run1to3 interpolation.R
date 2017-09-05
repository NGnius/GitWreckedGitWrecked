#PELT 1 Run 1 to 3 data check

#***********************************************************************************
#SET-UP####

  Working_Directory <- choose.dir()
  
  #Set working directory to:
  
  setwd(Working_Directory)
  
  if (getwd() == chartr("\\","/", as.character(Working_Directory))) {
    print.noquote(paste("Working directory is set to: <", chartr("\\","/", as.character(Working_Directory)),">"))
  } else {
    print("Selected working directory is not available")
  }
#***********************************************************************************
#IMPORT DATA####

  Run1to3_raw <- read.csv(file = "Pelt1_Run1to3.csv", header = TRUE)
  
    #verify
    Run1to3_raw
    
#***********************************************************************************
#COMPARISON OF TC AND UC [THg]s####

  #define variables
  TC_THg <- Run1to3_raw$TC_THg_ppm
  UC_THg <- Run1to3_raw$UC_THg_ppm
  
  #overall difference between TC and UC (paired)
  t.test(TC_THg, UC_THg, paired = TRUE, var.equal = FALSE, 
         alternative = "less")
#***********************************************************************************
#INTERPOLATION####

  #First, create individual stacked dataframes for TC and UC
  TC_THg_df <- cbind.data.frame(Run1to3_raw$X_coord, Run1to3_raw$Y_coord, Run1to3_raw$TC_THg_ppm)
    colnames(TC_THg_df) <- c("x", "y", "z") #lowercase letters
  UC_THg_df <- cbind.data.frame(Run1to3_raw$X_coord, Run1to3_raw$Y_coord, Run1to3_raw$UC_THg_ppm)
    colnames(UC_THg_df) <- c("x", "y", "z") #lowercase letters
    
    #verify
    TC_THg_df
    UC_THg_df
  
  #UNSTACK METHOD####
    library(reshape2)
    
  #Generate matrix for TC
  TC_THg_matrix <- acast(TC_THg_df, x~y, value.var = "z")
  TC_THg_matrix
  
  #Generate matrix for UC
  UC_THg_matrix <- acast(UC_THg_df, x~y, value.var = "z")
  UC_THg_matrix
  
#***********************************************************************************  
#RASTER INTERPOLATION####
  par(mfrow = c(1,2), las = 1)
  
  library(akima)
  #+Topcoat raster image####
  #generate coordinates of TC THg concentrations via interpolation of data points 
    #from z values
  TC_THg_interpMatrix <- interp(TC_THg_df$x,TC_THg_df$y, TC_THg_df$z, 
                          #use spline interpolation
                          linear = FALSE, 
                          #RASTER RESOLUTION
                          #number of divisions of x range
                          nx = 40, 
                          #number of divisions of y range
                          ny = 25)
  
  #verify interpolation matrix
  TC_THg_interpMatrix
  
    library(fields)
    library(viridis)
  
  #create raster image file with TC [THg] data
  image.plot(TC_THg_interpMatrix, xlim = range(TC_THg_df$x), ylim = range(TC_THg_df$y), 
        col = viridis(20), 
        #main plot parameters
        main = "PELT 1 TC THg distribution", xlab = "X distance from origin (cm)", 
        ylab = "Y distance from origin (cm)", useRaster = TRUE, asp = 1, 
        axes = TRUE, 
        #legend parameters
        horizontal = TRUE, 
        legend.lab = "[THg] (ppm)", 
        legend.shrink = 0.5)
    
    #generate custom axes
    #axis(1, at = seq(-20,5,5), labels = TRUE, pos = -100)
    #axis(2, at = seq(-125, 75, 25), labels = TRUE, pos = -30)
      
    
    #+Plot original sampling locations####
      #Dorsal points as circles
      #points(x= TC_THg_df$x, y = TC_THg_df$y) 
    
      #Ventral points as squares
      #points(x= TC_THg_df$x, y = TC_THg_df$y)
      
      #Furline points as triangles
      #points(x= TC_THg_df$x, y = TC_THg_df$y)
      
    #+Undercoat raster image####
    #generate coordinates of TC THg concentrations via interpolation of data points 
    #from z values
    UC_THg_interpMatrix <- interp(UC_THg_df$x,UC_THg_df$y, UC_THg_df$z, 
                                  #use spline interpolation
                                  linear = FALSE, 
                                  #RASTER RESOLUTION
                                  #number of divisions of x range
                                  nx = 40, 
                                  #number of divisions of y range
                                  ny = 25)
    
    #verify interpolation matrix
    UC_THg_interpMatrix
    
    #create raster image file with UC [THg] data
    image.plot(UC_THg_interpMatrix, xlim = range(UC_THg_df$x), ylim = range(UC_THg_df$y), 
          col = viridis(20), 
          #main plot parameters
          main = "PELT 1 UC THg distribution", xlab = "X distance from origin (cm)", 
          ylab = "Y distance from origin (cm)", useRaster = TRUE, asp = 1, 
          axes = TRUE, 
          #legend parameters
          horizontal = TRUE, 
          legend.lab = "[THg] (ppm)", 
          legend.shrink = 0.5)
    
    #generate custom axes
    #axis(1, at = seq(-20,5,5), labels = TRUE, pos = -100)
    #axis(2, at = seq(-125, 75, 25), labels = TRUE, pos = -30)
    
    #plot original sampling locations
      points(x= UC_THg_df$x, y = UC_THg_df$y)
      
    
#***********************************************************************************
#PLOT TRANPOSED MATRICES#####

    #Transposed TC [THg] matrix####
    as.matrix(TC_THg_interpMatrix_T)
    TC_THg_interpMatrix_T[[1]] <- TC_THg_interpMatrix$y
    TC_THg_interpMatrix_T[[2]] <- TC_THg_interpMatrix$x
    TC_THg_interpMatrix_T[[3]] <- t(TC_THg_interpMatrix$z)
    
    
    #create raster image file with TC [THg] data
    image(TC_THg_interpMatrix_T, xlim = range(TC_THg_df$y), ylim = range(TC_THg_df$x), 
          col = viridis(20), 
          main = "PELT 1 TC [THg] (ppm) {transposed}", xlab = "Y distance from origin (cm)", 
          ylab = "X distance from origin (cm)", useRaster = TRUE, asp = 1, 
          axes = TRUE)
    
    #generate custom axes
    #axis(2, at = seq(-20,5,5), labels = TRUE, pos = -100)
    #axis(1, at = seq(-100, 50, 25), labels = TRUE, pos = -30)
    
    #plot original sampling locations
    #points(x= TC_THg_df$y, y = TC_THg_df$x)
    
    library(fields)
    image.plot(TC_THg_interpMatrix_T, legend.only=T, col = viridis(20))
    