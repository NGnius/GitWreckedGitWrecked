#List Box With A Scroll Bar
  #from: http://mcu.edu.tw/~chenmh/teaching/project/r/reference/RTclTkExamples/listboxes.html

require(tcltk)
tt<-tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
                   command=function(...)tkyview(tl,...))
tl<-tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(tt,text="What's your favorite fruit?"))
tkgrid(tl,scr)
tkgrid.configure(scr,rowspan=4,sticky="nsw")
fruits <- c("Apple","Orange","Banana","Pear","Cherry","Apricot","Peach")
for (i in (1:7))
{
  tkinsert(tl,"end",fruits[i])
}
tkselection.set(tl,2)  # Default fruit is Banana.  Indexing starts at zero.

OnOK <- function()
{
  fruitChoice <- fruits[as.numeric(tkcurselection(tl))+1]
  tkdestroy(tt)
  msg <- paste("Good choice! ",fruitChoice,"s are delicious!",sep="")
  tkmessageBox(message=msg)
  
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkfocus(tt)