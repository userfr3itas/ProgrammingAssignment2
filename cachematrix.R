## The makeCacheMatrix function creates a list containing four functions: 
##set the matrix, get the matrix, set the inverse matrix and get the inverse matrix

## The cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix
## function. If Inverse has already been calculated, it gets from the cache.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse matrix 
##in the cache via the setsolve function.

## makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) s<<-solve
  getsolve<-function() s
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}

## cacheSolve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s<-x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setsolve(s)
  s
}