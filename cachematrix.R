## There are two functions, makeCacheMatrix and cacheSolve.
## The function designed to aid understand Lexical Scoping and how it works in R

## This function call creates matrix based on numeric input.
## superassignment operator: <<- to have the mean assigned at a higher level environment then the calling function


makeCacheMatrix <- function(x = matrix()) {
## Matrix is set to NULL, using the assignment operator: <-, 
  m <- NULL
  set <- function(y) {

## superassignment operator: <<- to have the mean assigned at a higher level environment then the calling function
    x <<- y
    m <<- NULL
  }
  get <- function() x

  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
          
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that does an element by element input inverse of the "x" input matrix.
## If the "m" matrices is not null, it used the cached store value in a higher level environment in place of have the calculation performed every time the function is called.
##  This matrix checking method comes into aid when doing machine learning applications.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()

## if m is not Null the cached value will be used
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
        
 ## Solve is a builtin function that does a matrix inverser
        
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
