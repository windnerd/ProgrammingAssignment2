## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 # This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # initialize
    m <- NULL
    
    # create a sub function set
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # create a get function
    get <- function() x
    
    # create a set inverse function (cache the inverse)
    setinverse <- function(inv) m <<- inv
    
    # create a get inverse function
    getinverse <- function() m
    
    # return the list
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            # get the inverse
    m <- x$getinverse()
    
    # if not null, the inverse is cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if not cached, collect data 
    d1 <- x$get()
    
    # calculate inverse if not found in cache 
    # (assume d1 is always invertible)
    m <- solve(d1)
    
    # set the inverse
    x$setinverse(m)
    
    # return and display the inverse
    return(m)
}
