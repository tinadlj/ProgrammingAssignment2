## Put comments here that give an overall description of what your
## functions do
## Objective: Cache the inverse of a matrix to use it repeatedly 

## Write a short comment describing this function
## Cache the inverse of a matrix.
## The function has four functions inside: 
## set: set the matrix
## get: return the matrix stored
## setIMatrix: set the inverse matrix
## getIMatrix: get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setIMatrix<- function(inverseM) m<<-inverseM
      getIMatrix <-function() m
      list(set=set,get=get,setIMatrix=setIMatrix,getIMatrix=getIMatrix)

}


## Write a short comment describing this function
## This function is used to get the inverse matrix
## if the inverse matrix is in the cache, then get it by calling getMatrix function
## if the inverse matrix is not in the cache, then calculate it by calling solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getIMatrix()
      if(!is.null(m)){
            return(m)
      }
      data<-x$get()
      m<-solve(data,)
      x$setIMatrix()
      m
      
}
