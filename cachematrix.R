## https://github.com/Ankur-Nangia/ProgrammingAssignment2
## R Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL     
        set <- function(y) {                            ## set the values of the matrix.
                x <<- y
                i <<- NULL
        }
        get <- function() x                             ## get the values of the matrix.
        setinverse <- function(inverse) i <<- inverse   ## set it inverse.
        getinverse <- function() i                      ## get its inverse.
        list(set = set,                                 ## This list is the special "matrix" object.  
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache and skips the computaion.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                           ## Return a matrix that is the inverse of 'x'.
        if(!is.null(i)) {                             ## Checks for cached inverse.
                message("getting cached data")  
                return(i)
        }
        data <- x$get()                               ## If no cached inverse, then the function.
        i <- solve(data, ...)                         ## calculates and print.
        x$setinverse(i)
        i
}
