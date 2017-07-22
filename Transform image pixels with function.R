#ADDING IMAGE DISTORTION TO X,Y PIXEL COORDS

#************************************************************************************
#REQUIRED PACKAGE INSTALLATION####

#imager graphics package
#install.packages("imager", dependencies=TRUE)
#install.packages("spatstat", dependencies=TRUE)

#**********************************************************************************
#SET-UP####

Working_Directory <- choose.dir()

#Set working directory to:

setwd(Working_Directory)

if (getwd() == chartr("\\","/", as.character(Working_Directory))) {
  print.noquote(paste("Working directory is set to: <", chartr("\\","/", as.character(Working_Directory)),">"))
} else {
  print("Selected working directory is not available")
}

#**********************************************************************************
#ADD SIMPLE IMAGE DISTORTION FUNCTION####

library(imager)

  #import image####
  image1 <- imager::load.image(choose.files())
 
  #warp image according to a specific function####
  
    #generate imwarp function
    map_bend <- function(x,y) list(x = 500*sin(x/20),y = 0*y)
  
    #warp image
    image1_warped <- imwarp(image1, direction = "backward", map = map_bend, coordinates = "relative")

    plot(image1_warped, main = "image1_warped")
    
#**********************************************************************************
#EXPORT IMAGE TO WORKING DIRECTORY####

  save.image(image1_warped, file = "image1_warped.jpeg", quality = 1)
  
#**********************************************************************************
#DEFINE A NEW COORDINATE SYSTEM####
break    
    #Define a new coordinate system with 0 at the center of the image
    Xcc <- function(im) Xc(im) - width(im)/2
    Ycc <- function(im) Yc(im) - height(im)/2
    image1_NewCoords <- (Xcc(parrots)^2+Ycc(parrots)^2) < 100^2
    plot(parrots)
    highlight(px)
    
    #multi.sapply <- function(.) {
     # arglist <- match.call(expand.dots = FALSE)$.
      #var.names <- sapply(arglist, deparse)
      #has.name <- (names(arglist) != "")
      #var.names[has.name] <- names(arglist)[has.name]
      #arglist <- lapply(arglist, eval.parent, n = 2)
      #x <- arglist[[1]]
      #arglist[[1]] <- NULL
      #result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
      #colnames(result) <- var.names[-1]
      #return(result)
    #}
    
    #Merge two functions? --> DO NOT USE!####
    #CenterOrigin <- function(im) {
     # c(Xc(im) - width(im)/2, Yc(im) - height(im)/2)
    #}
    
    #image1_NewCoords <- CenterOrigin(image1)
    
    #plot(image1_NewCoords)

#DEFINE NEW COORDS ROUND 2####
    Center <- function(x,y) list(x=50*sin(y/50),y=0*y)
    
    library(imager)
    
    UseMethod(cimg())
    cimg()
    
    image1_shifted <- imager::imshift(image1, delta_x = -width(image1)/2,
                                      delta_y = -height(image1)/2)
    
    plot(image1_shifted)
    
    class(image1_shifted)
    class(image1)
    
    image1_centered <- pixel.grid(image1, standardise = TRUE)
    class(image1_centered)
    
    image1_shifted <- 2
    
    image1_centered <- 3
    