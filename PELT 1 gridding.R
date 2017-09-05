#PLOT LINES FOR PELT 4####

#************************************************************************************
#SET WORKING DIRECTORY####

Working_Directory <- choose.dir()

#Set working directory to:

setwd(Working_Directory)

if (getwd() == chartr("\\","/", as.character(Working_Directory))) {
  print.noquote(paste("Working directory is set to: <", chartr("\\","/", as.character(Working_Directory)),">"))
} else {
  print("Selected working directory is not available")
}
#*************************************************************************************
#IMPORT COORDS GENERATED IN EXCEL####

X_Y_Coordinates <- read.csv(file = "PELT_1_XYCOORDS.csv", header = TRUE)

X_Y_Coordinates

plot(X_Y_Coordinates, asp = 1, main = "PELT 1:   1:1.25 rectangular grid ratio", 
     pch = 0)

Minor_grid_lines <- abline(h = seq(-10,10,1.25), v = seq(-5,5,1), lwd = 0.2)

Major_grid_lines <- abline(h = seq(-10,10,2.5), v = seq(-5,5,1), lwd = 2)

#*************************************************************************************
#GENERIC 1:1 GRID####

X_Y_Coordinates_generic <- read.csv(file = "GENERIC_1to1_grid.csv", header = TRUE)

X_Y_Coordinates_generic

plot(X_Y_Coordinates_generic, asp = 1, main = "GENERIC 1:1 square grid ratio", 
     pch = 0)

Minor_grid_lines <- abline(h = seq(-10,10,1), v = seq(-5,5,1), lwd = 0.2)

Major_grid_lines <- abline(h = seq(-10,10,2), v = seq(-5,5,1), lwd = 2)
