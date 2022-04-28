source("simple_slot_function_file.R")#loading function file

#Creating a vector that is composed of two of each integer from 1-9. The vector
#is duplicated twice, each vector represents one reel of a slot machine.
set.seed(1)
reel.1<-sample(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 6, 7))
set.seed(2)
reel.2<-sample(reel.1)
set.seed(3)
reel.3<-sample(reel.2)

#Creating a matrix by combining the reel vectors, each column represent a reel
reel.matrix<-as.matrix(cbind(reel.1, reel.2, reel.3))

#creating a matrix where the rows correspond to the reels for i.e row 1 
#corresponds to reel 1. The columns correspond to the top, middle, and bottom 
#display spots for each reel. The elements correspond to the row # of the reel
#matrix.I.E reel.matrix[window.matrix[1,1],1], will show the top element of the 
#display spot for reel 1. 
window<-c(1, 1, 1, 2, 2, 2, 3, 3, 3)
window.matrix<-matrix(window,ncol=3,nrow=3)
SlotManager(reel.matrix,window.matrix)