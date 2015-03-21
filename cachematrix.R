##Following function creates a special "matrix" object that can 
##cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  inv_mat<-NULL     #initially setting the inverse matrix as NULL
  set<-function(y){
    x<<-y        #set the value of x to new matrix y   
    # "<<-" is used to assign value to a variable to show its defined in a new environment.
    inv_mat<<-NULL # resets inv_mat to NULL
  }
  get<-function()x  #returns the input matrix x
  setinv<-function(inv)inv_mat<-inv # this function sets inverse matrix to inv_mat
  getinv<-function()inv_mat  #returns inverse inv_mat        
  list(set=set,get=get,
       setinv=setinv,getinv=getinv) # this returns a list of above functions defined,
  # together with their corresponding environments.
  
  

}


##Following function computes the inverse of the special "matrix" returned 
##by "makeCachematrix.R".
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat<-x$getinv() #this gets inverse matrix of x 
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)#returns inverse
  }
  data <- x$get() #if condition falis in "if" then it gets "x" to calculate inverse.
  inv_mat <- solve(data, ...)# calculates the inverse
  x$setinv(inv_mat) # here it is set to the object x
  inv_mat # returns the inverse here.
}
# To see the output set the directory to your working directory
# source("makeCacheMatrix.R")
# source("cacheSolve.R")
# create a nonsingular matrix
#e.g ns_mat<-rbind(c(1, -1/2), c(-1/2, 1)) 
#det(mat_cache)
#0.75 # so it is nonsingular
#mat_cache<-makeCachematrix(ns_mat)
#cache_inv<-cacheSolve(mat_cache)
#if you want to test then cache_inv%*%ns_mat. it will show in identity matrix.