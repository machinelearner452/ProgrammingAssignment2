## There are two functions, makeVector and cachemean.
## The function designed to aid understand Lexical Scoping and how it works in R

## This function call creates vectors based on numeric input.
## Matric is set to NULL, using the assignment operator: <-, 
## superassignment operator: <<- to have the mean assigned at a higher level environment then the calling function


makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## cacheman is a function that does an element by element mean calculation of the "m" matric.
## If the "m" matrices is not null, it used the cached store value in a higher level environment in place of have the calculation performed every time the function is called.
##  This matric checking method comes into aid when doing machine learning applications.


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
