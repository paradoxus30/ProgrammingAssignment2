## Matrix inversion can be computationally intensive, hence it is better to use cached value when possible. The two functions here calculate the inverse of a matrix by first accessing the cached value, if present and thereby being computationally efficient. If there is no value available in the cache, it calculates the inverse matrix and stores it in the cache. Returns the inverse matrix of 'x'. 

## The first function, makeCacheMatrix creates a list containing a function to: 
## 1) set the value of the matrix 
## 2) get the value of the matrix 
## 3) set the value of the inverse matrix 
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Store the value of the inverse matrix in cache
        invmat <- NULL
        set <- function( y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invmat <<- inv
        getinv <- function() invmat
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function, cacheSolve checks to see if the matrix inverse has already been calculated. If it has, it skips the calculation and access the inverse from the cache. Otherwise, it calculates the inverse matrix and stores the value in the cache through the setinv function.

cacheSolve <- function(x, ...) { 
        ## Access the cache for the inverse matrix, if available. Otherwise, calculate it and store it in the cache. Return a matrix that is the inverse of 'x'
        invmat <- x$getinv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...) 
        x$setinv(invmat) 
        invmat
}
