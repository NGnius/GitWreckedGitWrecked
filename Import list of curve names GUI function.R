#IMPORT LIST OF SHAPE NAMES FUNCTION (COMPLETE)

#+Set shape names source file (mandatory)####
msgBox_ShapeNamesSource <- tk_messageBox(title = "Set shape names source file", 
                                         message = "Choose the shape names source file.
                                         
                                         The name source file must be a single column which contains only the same shape names used to name the pictures that were imported into the stereomorph landmark/curve digitization function in the .csv format. The correct format for the .csv file is presented below:
                                         
                                         <no column header>  <no other columns>
                                         shape_1_1
                                         shape_1_2
                                         shape_2_1
                                         shape_2_2
                                         etc.
                                         ")
ShapeNamesSource <- read.table(choose.files(caption = "Select the shape names .csv file"), 
                               sep = "", header = FALSE)

  #verify ShapeNamesSource import
  ShapeNamesSource

#create new column in the ShapeNamesSource table which adds the ".txt" filetype
#to each shape name (so that the file.choose will know which type of file 
#type to look for in the working directory)

#col number for new last column *keep this outside of the loop!
col_ShapeNamesSource <- c(ncol(ShapeNamesSource)+1)

#loop
for(i in 1:c(nrow(ShapeNamesSource))) {
  ShapeNamesSource[i,col_ShapeNamesSource] <- paste(ShapeNamesSource[i,1], ".txt", sep = "")
}

#rename all columns
colnames(ShapeNamesSource) <- c("name", "name.txt")

  #verify 
  ShapeNamesSource

