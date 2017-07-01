## Both functions work together to create a invertible matrix object
## and make the inverse of the matrix available in the cache

## Computing the inverse of a matrix  with the solve function
## the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 ## set cache to NULL
    set <- function(y) {        ##create the matrix 
        x <<- y
        inv <<- NULL
    }
    get <- function() x         ## get value matrix
    setinv <- function(inverse) inv <<- inverse ##invert and store in cache
    getinv <- function() inv    ## get from cache
        list(set = set,         ## return list with functions.
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the invertible matrix 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cacheSolve function retrieve the inverse from the cache.
## Computing the inverse of a matrix with the solve function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()           ##try to get the inverse from cache
    if (!is.null(inv)) {        ## if the inverse of x exists,  
        message("getting cached data")
        return(inv)             ## return the inverse in the console
    }
    matrx <- x$get()            ## else create the matrix, as it not exists
    inv <- solve(matrx, ...)    ## invert the matrix 
    x$setinv(inv)               ## set it in the cache
    inv                         ## and return it in the console.
}
