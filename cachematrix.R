## JHU Coursera R Programming W3 Assignment #2
## July 2017
## submitted by: JNLWH (jnlatwork@gmail.com)

## Invert a matrix and cache its result, so subsequent requests for matrix inversion
## retrieve from cache rather than re-calculate from scratch

## about makeCacheMatrix
## part 1
## initialize objects x and im. x is a function argument.
## part 2
## set "new" values for the cached inversion and retrieve x from parent environment
## part 3
## defines setter for solve im and retrieve value from parent environment
## part 4
## create list with methods for get/set of both original matrix and its inverse

## part 1
makeCacheMatrix <- function(x = matrix()) {  
        im <- NULL 
## part 2
        set <- function(y) { 
                x <<- y
                im <<- NULL
        }
        get <- function() x
## part 3
        setsolve <- function(solve) im <<- solve 
        getsolve <- function() im 
## part 4
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns a matrix that's the inverse of x

cacheSolve <- function(x, ...) {
        im <- x$getsolve()
        if(!is.null(im)) {
                message("getting cached solve")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setsolve(im)
        im
}

## below is a test
a <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)  ## a is a 2x2 matrix
b <- solve(a)                                   ## b is inverse of a

c <- matrix(c(2, 6, 9, 4), nrow = 2, ncol = 2)  ## c is a 2x2 matrix
d <- solve(c)                                   ## d is inverse of c

## testing functions makeCacheMatrix and cacheSolve
aMatrix <- makeCacheMatrix(a)
aMatrix <- makeCacheMatrix(c)
cacheSolve(aMatrix)

## alternative step-by-step calculation
aMatrix <- makeCacheMatrix(a)
aMatrix
aMatrix$get()
aMatrix$getsolve()
aMatrix$set(c)
cacheSolve(aMatrix)
aMatrix$getsolve()
d
