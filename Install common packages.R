##################################################################################
#INSTALL PACKAGES

#lmPerm2 package for linear regression permutation tests and comparison of samples
# Windows package installation command:
install.packages("http://www.antoinemorin.com/biostats/2015/lmPerm2_1.0.zip", 
                 repos = NULL, type = "win.binary")

#ggplot2 plotting package
install.packages("ggplot2", dependencies=TRUE)

#regression package
install.packages("car", dependencies=TRUE)

#Bootstrapping package
install.packages("boot", dependencies=TRUE)

#linear regression package
install.packages("lmtest", dependencies=TRUE)

#multiple comparisons package
install.packages("multcomp", dependencies=TRUE)

#linear model permutations package
install.packages("multcomp", dependencies=TRUE)

#one-way ANOVA tests, including Brown-Forsythe one-way method for heterogeneity of
  #variance
install.packages("onewaytests", dependencies=TRUE)

#MASS package (step-wise multiple regression package)
install.packages("MASS", dependencies = TRUE)

#MuMIn package (information theoreical approach for selection of best set of
  #multiple linear regression models)
install.packages(pkgs = "MuMIn", dependencies = TRUE)

#simpleboot package (Bootstrap analysis -> useful for generating CIs from the
  #empirical percentiles in a multiple linear regression)
install.packages(pkgs = "simpleboot", dependencies = TRUE)

#StereoMorph 2D/3D morphometrics landmarking package
install.packages("StereoMorph", dependencies=TRUE)

#Geomorph 2D/3D morphometrics analysis package
install.packages("StereoMorph", dependencies=TRUE)

#geoR plotting package (not available)
install.packages("geoR", dependencies=TRUE)

#Colour palette package (safe for colourblindness, etc)
install.packages("viridis", dependencies = TRUE)

#akima package 
install.packages("akima", dependencies = TRUE)

#reshape2 package for converting between data organisation types 
  #such as stacked and unstacked matrices
install.packages("reshape2", dependencies = TRUE)

#RSAGA GIS and spatial data package
  #has grid matrix to xyz coordinate conversion function for stacking matrices
install.packages("RSAGA", dependencies = TRUE)


##################################################################################

#important packages for BIO4158 course
