## 
## Programming assignment 2
## Subhendu Mohanty, 22/05/2015
##

## This assignment has two functions - makeCacheMatrix and cacheSolve:

## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
##                   It returns a list of functions as below:

## setmatrix - this function takes an invertible matrix as argument and returns the same 
## getmatrix - this function returns the invertible matrix from cache, as set by setmatrix, doesnt need any argument
## setmatrix_in - this  function takes the invertible matrix as an argument and calculates and returns its inverse
## getmatrix_in - this function doesnt need arguments, it returns the cached matrix inverse as set by setmatrix_inv
 
makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                matrix_inv <<- NULL
        }
        getmatrix <- function() x
        setmatrix_inv <- function(x) matrix_inv <<- solve(x)
        getmatrix_inv <- function() matrix_inv
        list(set = setmatrix , get = getmatrix ,
	       setinv = setmatrix_inv,getinv = getmatrix_inv)
}


## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
##	          inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##		    retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }
        data <- x$get()
        x$setinv(data)
        m <- x$getinv()
}
