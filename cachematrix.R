## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function that creates a Special 'matrix' that cachse its inverse 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 # initialize inv as NULL 
    
    set <- function(y) {        # set function to assign new 
        x <<- y                 # value of matrix in parent environment
        inv <<- NULL            # new matrix ->  reset inv to NULL
    }
    get <- function() x         # get fucntion -> returns matrix argument value
    
    # assigns value of inv in parent environment
    sinv <- function(inverse) inv <<- inverse  
    
    # gets the value of inv where called
    ginv <- function() inv                     
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)  

}


## Write a short comment describing this function

# function computes inverse of special "matrix" returned by makeCacheMatrix 
# if inverse has already been calculated and the matrix has not changed
# then, cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinverse(inv)

    inv
}
