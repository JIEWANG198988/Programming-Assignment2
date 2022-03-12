## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
          i<-NULL
  ## Method to set the matrix
          set<-function(y){
            x<<-y
            i<<-NULL
          }
          ## Method the get the matrix
          get<-function() x
          
          ## Method to set the inverse of the matrix
          setinverse<-function(inverse) i<<-inverse
          ## Method to get the inverse of the matrix
          getinverse<-function() i
          ## Return a list of the methods
          list(set=set,get=get,
               setinverse=setinverse,
               getinverse=getinverse)
}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not changed)
## Then the "casheSolve"should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        
        ## Just return the inverse if its already set
        if(!is.null(i)){
          message("geting cached data")
          return(i)
        }
        
        ## Get the matrix from our object
        data<-x$get()
        
        ## Caculate the inverse using matrix multiplication
        i<-solve(data,...)
        
        ##Set the inverse to the object
        x$setinverse(i)
        
        ## Retun the matrix
        i
}
