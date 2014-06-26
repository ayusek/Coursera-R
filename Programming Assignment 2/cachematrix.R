## Put comments here that give an overall description of what your
## functions do

## This function makes a matrix that can cache its inverse. The function has been made similar to the makevector function defined in the program.

makeCacheMatrix <- function(x = matrix()) {
    #initializing the inverse of the matrix property
    m <- NULL
    
    #Method to set the matrix. Set up in a different enviornment with the help of <<- operator
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #Getting the data(Matrix)
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. Inspired from the example given in the problem.
        m <- x$getInverse()
        #if we found a valid data then return it
        if(!is.null(m))
        {
            message("getting cached data")
            return(m)
            
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data)
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
        
        ## I have not handled the case when the matrix is dynamically changed. This was not asked in the problem but is an important aspect to be dealt with.
        
}
