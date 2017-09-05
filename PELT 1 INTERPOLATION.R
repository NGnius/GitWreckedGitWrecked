#PELT 1 INTERPOLATION

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

  PELT1_raw <- read.csv(file = "PELT1.csv", header = TRUE)
  
    #verify
    PELT1_raw

#***********************************************************************************
#COMPARISON OF TC AND UC [THg]s####

  #define variables
    TC_THg <- PELT1_raw$THg_TC_ppm
    UC_THg <- PELT1_raw$THg_UC_ppm
  
  #check assumptions
    hist(TC_THg, freq = FALSE, breaks = 30, xlab = "[THg] (ppm)", 
         main = "Histogram of TC [THg]") 
    curve(dnorm(x, mean(TC_THg), sd(TC_THg)), add=TRUE, col="darkblue", lwd=2)
    qqnorm(TC_THg, main = "PELT 1 TC [THg] normal Q-Q plot")
    qqline(TC_THg)
    
    hist(UC_THg, freq = FALSE, breaks = 30, xlab = "[THg] (ppm)")
    curve(dnorm(x, mean(UC_THg), sd(UC_THg)), add=TRUE, col="darkblue", lwd=2)
    qqnorm(UC_THg, main = "PELT 1 UC [THg] normal Q-Q plot")
    qqline(UC_THg)
  
  #overall difference between TC and UC (paired)
    t.test(TC_THg, UC_THg, paired = TRUE, var.equal = FALSE, 
           alternative = "less")
  
  
  
#***********************************************************************************
#INTERPOLATION####

  #First, create individual stacked dataframes for TC and UC
  THg_TC_df <- cbind.data.frame(PELT1_raw$X_coord, PELT1_raw$Y_coord, PELT1_raw$THg_TC_ppm)
  colnames(THg_TC_df) <- c("x", "y", "z") #lowercase letters
  THg_UC_df <- cbind.data.frame(PELT1_raw$X_coord, PELT1_raw$Y_coord, PELT1_raw$THg_UC_ppm)
  colnames(THg_UC_df) <- c("x", "y", "z") #lowercase letters
  
  #verify
  THg_TC_df
  THg_UC_df

#UNSTACK METHOD####
  library(reshape2)
  
  #Generate matrix for TC
  THg_TC_matrix <- acast(THg_TC_df, x~y, value.var = "z")
  THg_TC_matrix

  #Generate matrix for UC
  THg_UC_matrix <- acast(THg_UC_df, x~y, value.var = "z")
  THg_UC_matrix

#***********************************************************************************  
#RASTER INTERPOLATION####
par(mfrow = c(1,1), las = 1, mar = c(3, 2, 4, 2) + 0.1)

library(akima)
  
  #+Topcoat raster image####
    #generate coordinates of TC THg concentrations via interpolation of data points 
    #from z values
    TC_THg_interpMatrix <- interp(THg_TC_df$x,THg_TC_df$y, THg_TC_df$z, 
                                  #use spline interpolation
                                  linear = TRUE, extrap = FALSE,
                                  #RASTER RESOLUTION
                                  #number of divisions of x range
                                  nx = 28, 
                                  #number of divisions of y range
                                  ny = 84)
    
    #verify interpolation matrix
    TC_THg_interpMatrix
    
    library(fields)
    library(viridis)
    
    par(mfrow = c(1,1), las = 1, mar = c(5,1,5,1) + 0.1, xpd = TRUE, pin = c(3.25,3))
    
    #create raster image file with TC [THg] data
    image.plot(TC_THg_interpMatrix, xlim = range(THg_TC_df$x), ylim = range(THg_TC_df$y), 
               zlim = range(THg_UC_df$z), col = viridis(20), 
               #main plot parameters
               main = "PELT 1 TC THg distribution", xlab = "X distance from origin (cm)", 
               ylab = "Y distance from origin (cm)", useRaster = TRUE, asp = 1, 
               axes = TRUE, 
               #legend parameters
               horizontal = FALSE, 
               legend.lab = "  [THg] (ppm)", 
               legend.shrink = 0.5, 
               legend.args = list(text("ppm"), cex = 1.5, side = 4, line = 2),
               #adjust legend location by coords 
               smallplot= c(0.80,0.825,0.25,0.75)
               )
    
    #generate custom axes
    #axis(1, at = seq(-20,5,5), labels = TRUE, pos = -100)
    #axis(2, at = seq(-125, 75, 25), labels = TRUE, pos = -30)
    
    
  #+Plot original sampling locations####
    #Dorsal points as circles
    points(x = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Dorsal",
                                        select = X_coord))), 
           y = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Dorsal",
                                        select = Y_coord))), 
           pch = 1) 
    
    #Ventral points as squares
    points(x = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Ventral",
                                        select = X_coord))), 
           y = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Ventral",
                                        select = Y_coord))), 
           pch = 0) 
    
    #FurLine points as triangles
    points(x = as.numeric(unlist(subset(PELT1_raw,Fur_region=="FurLine",
                                        select = X_coord))), 
           y = as.numeric(unlist(subset(PELT1_raw,Fur_region=="FurLine",
                                        select = Y_coord))), 
           pch = 2) 
    
      #++Fur_region legend####
      legend(x = c(25,70), y = c(-20,30), legend = c("Dorsal", "Ventral", "Fur Line"), 
             pch = c(1,0,2), title = "Fur region", horiz = FALSE,
             #text parameters 
             text.width = 2, xjust = 0)

    
    
    par(mfrow = c(1,1), las = 1, mar = c(5,1,5,1) + 0.1, xpd = TRUE, pin = c(3.25,3))
    
  #+Undercoat raster image####
    #generate coordinates of TC THg concentrations via interpolation of data points 
    #from z values
    UC_THg_interpMatrix <- interp(THg_UC_df$x,THg_UC_df$y, THg_UC_df$z, 
                                  #use spline interpolation
                                  linear = TRUE, extrap = FALSE,
                                  #RASTER RESOLUTION
                                  #number of divisions of x range
                                  nx = 28, 
                                  #number of divisions of y range
                                  ny = 84)
    
    #verify interpolation matrix
    UC_THg_interpMatrix
    
    #create raster image file with UC [THg] data
    image.plot(UC_THg_interpMatrix, xlim = range(THg_UC_df$x), ylim = range(THg_UC_df$y),
               zlim = range(THg_UC_df$z), col = viridis(20), 
               #main plot parameters
               main = "PELT 1 UC THg distribution", xlab = "X distance from origin (cm)", 
               ylab = "Y distance from origin (cm)", useRaster = TRUE, asp = 1, 
               axes = TRUE, 
               #legend parameters
               horizontal = FALSE, 
               legend.lab = "  [THg] (ppm)", 
               legend.shrink = 0.5, 
               legend.args = list(text("ppm"), cex = 1.5, side = 4, line = 2),
               #adjust legend location by coords 
               smallplot= c(0.80,0.825,0.25,0.75)
               )
    
    #generate custom axes
    #axis(1, at = seq(-20,5,5), labels = TRUE, pos = -100)
    #axis(2, at = seq(-125, 75, 25), labels = TRUE, pos = -30)
    
  #+Plot original sampling locations####
    #Dorsal points as circles
    points(x = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Dorsal",
                                        select = X_coord))), 
           y = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Dorsal",
                                        select = Y_coord))), 
           pch = 1) 
    
    #Ventral points as squares
    points(x = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Ventral",
                                        select = X_coord))), 
           y = as.numeric(unlist(subset(PELT1_raw,Fur_region=="Ventral",
                                        select = Y_coord))), 
           pch = 0) 
    
    #FurLine points as triangles
    points(x = as.numeric(unlist(subset(PELT1_raw,Fur_region=="FurLine",
                                        select = X_coord))), 
           y = as.numeric(unlist(subset(PELT1_raw,Fur_region=="FurLine",
                                        select = Y_coord))), 
           pch = 2) 
    
      #++Fur_region legend####
      legend(x = c(25,70), y = c(-20,30), legend = c("Dorsal", "Ventral", 
                                                     "Fur Line"), 
             pch = c(1,0,2), title = "Fur region", horiz = FALSE,
             #text parameters 
             text.width = 2, xjust = 0)

#***********************************************************************************
#STATISTICAL ANALYSES####

  #+One-way ANOVA for Anatomical_region factor####
    
    #++Residual diagnostics for TC [THg]####
      
      par(mfrow = c(2,2), mar = c(5,5,5,5))
      
      pelt1_TC_anova1 <- lm(THg_TC_ppm ~ Anatomical_region, data = PELT1_raw)
      plot(pelt1_TC_anova1, main = "TC [THg] by anatomical region")
      
        #normality test of residuals
        shapiro.test(residuals(pelt1_TC_anova1))
        
        par(mfrow = c(1,1), las = 1, mar = c(5,5,5,5))
        hist(residuals(pelt1_TC_anova1), breaks = 30, freq = FALSE, 
             main = "Histogram of PELT 1 TC [THg] one-way ANOVA residuals by anatomical region")
        curve(dnorm(x, mean(residuals(pelt1_TC_anova1)), sd(residuals(pelt1_TC_anova1))), 
              add=TRUE, col="darkblue", lwd=2)
        
          
        #homoscedasticity test of residuals
        library(car)
        leveneTest(THg_TC_ppm ~ Anatomical_region, data = PELT1_raw)
        
        #pelt anova results
        summary(pelt1_TC_anova1)
        
      #Tukey's multiple comparison
        #set up ANOVA 
        pelt1_TC_anova1.ANOVA <- aov(THg_TC_ppm ~ Anatomical_region, data = PELT1_raw)
        
        #Tukey's multiple comparison
        TukeyHSD(pelt1_TC_anova1.ANOVA)
        
        #plot Tukey's
        library(multcomp)
          
        pelt1_TC_anova1.means <- glht(pelt1_TC_anova1.ANOVA, 
                                      linfct = mcp(Anatomical_region = "Tukey"))
          
        CI_pelt1_TC_anova1.means <- cld(pelt1_TC_anova1.means)
          
          #plot settings
          par(mar = c(5,5,9,5))
          plot(CI_pelt1_TC_anova1.means, 
                 main = "Tukey's multiple comparison for PELT 1 TC [THg] by anatomical region",
                 xlab = "Anatomical region", ylab = "[THg] (ppm)")
      
 
    #++Residual diagnostics for UC [THg]####
      
        par(mfrow = c(2,2), mar = c(5,5,5,5))
        
        pelt1_UC_anova1 <- lm(THg_UC_ppm ~ Anatomical_region, data = PELT1_raw)
        plot(pelt1_UC_anova1, main = "UC [THg] by anatomical region")
        
        #normality test of residuals
        shapiro.test(residuals(pelt1_UC_anova1))
        
        par(mfrow = c(1,1), las = 1, mar = c(5,5,5,5))
        hist(residuals(pelt1_UC_anova1), breaks = 30, freq = FALSE, 
             main = "Histogram of PELT 1 UC [THg] one-way ANOVA residuals by anatomical region")
        curve(dnorm(x, mean(residuals(pelt1_UC_anova1)), sd(residuals(pelt1_UC_anova1))), 
              add=TRUE, col="darkblue", lwd=2)
        
        
        #homoscedasticity test of residuals
        library(car)
        leveneTest(THg_UC_ppm ~ Anatomical_region, data = PELT1_raw)
        
        #pelt anova results
        summary(pelt1_UC_anova1)
        
      #Tukey's multiple comparison
        #set up ANOVA 
        pelt1_UC_anova1.ANOVA <- aov(THg_UC_ppm ~ Anatomical_region, data = PELT1_raw)
        
        #Tukey's multiple comparison
        TukeyHSD(pelt1_UC_anova1.ANOVA)
        
        
        #plot Tukey's
        library(multcomp)
        
        pelt1_UC_anova1.means <- glht(pelt1_UC_anova1.ANOVA, 
                                      linfct = mcp(Anatomical_region = "Tukey"))
        
        CI_pelt1_UC_anova1.means <- cld(pelt1_UC_anova1.means)
        
          #plot settings
          par(mar = c(5,5,9,5))
          plot(CI_pelt1_UC_anova1.means, 
               main = "Tukey's multiple comparison for PELT 1 UC [THg] by anatomical region",
               xlab = "Anatomical region", ylab = "[THg] (ppm)")
    
#*****    
  #+One-way ANOVA for Fur_region factor####
      
    #++Residual diagnostics for TC [THg]####
        
        par(mfrow = c(2,2), mar = c(5,5,5,5))
        
        pelt1_TC_anova2 <- lm(THg_TC_ppm ~ Fur_region, data = PELT1_raw)
        plot(pelt1_TC_anova2, main = "TC [THg] by fur region")
        
        #normality test of residuals
        shapiro.test(residuals(pelt1_TC_anova2))
        
        par(mfrow = c(1,1), las = 1, mar = c(5,5,5,5))
        hist(residuals(pelt1_TC_anova2), breaks = 30, freq = FALSE, 
             main = "Histogram of PELT 1 TC [THg] one-way ANOVA residuals by fur region")
        curve(dnorm(x, mean(residuals(pelt1_TC_anova2)), sd(residuals(pelt1_TC_anova2))), 
              add=TRUE, col="darkblue", lwd=2)
        
        
        #homoscedasticity test of residuals
        library(car)
        leveneTest(THg_TC_ppm ~ Fur_region, data = PELT1_raw)
        
        #pelt anova results
        summary(pelt1_TC_anova2)
        
      #Tukey's multiple comparison
        #set up ANOVA 
        pelt1_TC_anova2.ANOVA <- aov(THg_TC_ppm ~ Fur_region, data = PELT1_raw)
        
        #Tukey's multiple comparison
        TukeyHSD(pelt1_TC_anova2.ANOVA)
        
        #plot Tukey's
        library(multcomp)
        
        pelt1_TC_anova2.means <- glht(pelt1_TC_anova2.ANOVA, 
                                      linfct = mcp(Fur_region = "Tukey"))
        
        CI_pelt1_TC_anova2.means <- cld(pelt1_TC_anova2.means)
        
          #plot settings
          par(mar = c(5,5,9,5))
          plot(CI_pelt1_TC_anova2.means, 
               main = "Tukey's multiple comparison for PELT 1 TC [THg] by fur region",
               xlab = "Fur region", ylab = "[THg] (ppm)")
        
        
    #++Residual diagnostics for UC [THg]####
        
        par(mfrow = c(2,2), mar = c(5,5,5,5))
        
        pelt1_UC_anova2 <- lm(THg_UC_ppm ~ Fur_region, data = PELT1_raw)
        plot(pelt1_UC_anova2, main = "UC [THg] by fur region")
        
        #normality test of residuals
        shapiro.test(residuals(pelt1_UC_anova2))
        
        par(mfrow = c(1,1), las = 1, mar = c(5,5,5,5))
        hist(residuals(pelt1_UC_anova2), breaks = 30, freq = FALSE, 
             main = "Histogram of PELT 1 UC [THg] one-way ANOVA residuals by fur region")
        curve(dnorm(x, mean(residuals(pelt1_UC_anova2)), sd(residuals(pelt1_UC_anova2))), 
              add=TRUE, col="darkblue", lwd=2)
        
        
        #homoscedasticity test of residuals
        library(car)
        leveneTest(THg_UC_ppm ~ Fur_region, data = PELT1_raw)
        
        #pelt anova results
        summary(pelt1_UC_anova2)
        
      #Tukey's multiple comparison
        #set up ANOVA 
        pelt1_UC_anova2.ANOVA <- aov(THg_UC_ppm ~ Fur_region, data = PELT1_raw)
        
        #Tukey's multiple comparison
        TukeyHSD(pelt1_UC_anova2.ANOVA)
        
        
        #plot Tukey's
        library(multcomp)
        
        pelt1_UC_anova2.means <- glht(pelt1_UC_anova2.ANOVA, 
                                      linfct = mcp(Fur_region = "Tukey"))
        
        CI_pelt1_UC_anova2.means <- cld(pelt1_UC_anova2.means)
        
          #plot settings
          par(mar = c(5,5,9,5))
          plot(CI_pelt1_UC_anova2.means, 
               main = "Tukey's multiple comparison for PELT 1 UC [THg] by fur region",
               xlab = "Fur region", ylab = "[THg] (ppm)")
        
