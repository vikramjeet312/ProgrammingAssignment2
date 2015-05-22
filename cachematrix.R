## This program takes a "special" matrix type as defined by the first
## function and caches its inverse, to save time doing this operation

## makeCacheMatrix defines the matrix with various getters and setters

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<- function(y,nrow,ncol){
    x<<- matrix(y,nrow,ncol)
    inverse<<-NULL
  }
  get<- function() x
  setinverse<- function(inv) inverse<<-inv
  getinverse<- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve returs the inverse of the given matrix either by
## cheching cached data, if available, or by calculating it

cacheSolve <- function(x, ...) {
  inverse<- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  else{
    data<- x$get()
    inverse<- solve(data)
    x$setinverse(inverse)
    return(inverse)
  }
}
