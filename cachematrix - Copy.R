## Put comments here that give an overall description of what your
## functions do
##This function creates a matrix and calculates its inverse.

## Write a short comment describing this function
##This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse
)
}

## Write a short comment describing this function
## This function calculates the inverse of the generated matrix. If the inverse has 
##already been calculated, the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    print("Getting the inverse from the store!")
    return(m)
  }
  inv<-x$get()
  m<-solve(inv)
  x$setinverse(m)
  m
}
