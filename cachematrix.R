## functions that cache the inverse of a matrix.

## makeCacheMAtrix:This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversem<-NULL
  set<-function(y){
    x<<-y
    inversem<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) inversem<<-solve
  getinverse<-function() inversem
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inversem<-x$getinverse()
        if(!is.null(inversem)){
          message("getting cached data")
          return(inversem)
        }
        data<-x$get()
        inversem<-solve(data,...)
        x$setinverse(inversem)
        inversem
}
