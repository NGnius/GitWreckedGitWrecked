#PELT 1 Run 1 data check

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
#VERIFY DATA####
  
  
  #check for visual patterns in THg distributions
  hist(Run1to3_raw$TC_THg.ng.g., main = "Frequency of TC THg concentrations", 
       xlab = "[THg] (ng/g)")
  
  qqnorm(Run1to3_raw$TC_THg.ng.g., main = "Normal Q-Q plot for TC [THg]")
    qqline(Run1to3_raw$THg.topcoat..TC...ng.g.)
  
  plot(Run1to3_raw$Y_coord, Run1to3_raw$THg.topcoat..TC...ng.g., 
       xlab = "Distance from origin (cm)", ylab = "Topoat [THg] (ng/g)", 
       main = "Variation of Topcoat [THg]s with distance on -17.2cm pelt column")
  
  
  
  hist(Run1to3_raw$THg.undercoat..UC...ng.g., main = "Frequency of UC THg concentrations", 
       xlab = "[THg] (ng/g)")
  
  qqnorm(Run1to3_raw$THg.undercoat..UC...ng.g., main = "Normal Q-Q plot for UC [THg]")
    qqline(Run1to3_raw$THg.undercoat..UC...ng.g.)
    
  plot(Run1to3_raw$Y_coord, Run1to3_raw$THg.undercoat..UC...ng.g., 
       xlab = "Distance from origin (cm)", ylab = "Undercoat [THg] (ng/g)", 
       main = "Variation of Undercoat [THg]s with distance on -17.2cm pelt column")
  
  
  #check for equal variances and standard deviations
  var(Run1to3_raw$THg.topcoat..TC...ng.g.)
  
  var(Run1to3_raw$THg.undercoat..UC...ng.g.)
  
#***********************************************************************************
#COMPARISON OF TC AND UC [THg]s####
  
  #define variables
  TC_THg <- Run1to3_raw$TC_THg.ng.g.
  UC_THg <- Run1to3_raw$UC_THg.ng.g.
  
  
  #overall difference between TC and UC (unpaired)
  t.test(TC_THg, UC_THg, paired = FALSE, var.equal = FALSE, 
         alternative = "less")
  
  #overall difference between TC and UC (paired)
  t.test(TC_THg, UC_THg, paired = TRUE, var.equal = FALSE, 
         alternative = "less")
  
#***********************************************************************************
#INTERPOLTATIONS####  
  
  #Import fake coord data 
  Run1_coords <- read.csv(file = "Pelt1_Run1_fake_coords.csv", header = TRUE)
  
  library(akima)
  library(viridis)
  
  #Topcoat####
  #Assign variable name
  Run1_TC_THg <- cbind.data.frame(Run1_coords$X_coord, Run1_coords$Y_coord, 
                                  Run1_coords$THg.topcoat..TC...ng.g.)
    colnames(Run1_TC_THg) <- c("X", "Y", "Topcoat_THg")
    
    #verify
    Run1_TC_THg
    
  
  #Undercoat####
  #Assign variable name
  Run1_UC_THg <- cbind.data.frame(Run1_coords$X_coord, Run1_coords$Y_coord, 
                                  Run1_coords$THg.undercoat..UC...ng.g.)
    colnames(Run1_UC_THg) <- c("X", "Y", "Undercoat_THg")
    
    #verify
    Run1_UC_THg
