##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function()x
    ## if the target matrix is a square matrix
    ## then compute its inverse and try to cache it
    
    if (nrow(x) == ncol(x)) {
        setinverse <- function(solve) im <<- solve
        getinverse <- function()im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }
    ## if the target matrix is not a square matrix
    ## stop the script
    
    else {
        stop("the target matrix is not a square matrix")
        exit()
    }
   
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed) 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if x is a square matrix, calculate the inverse matrix 
        ## use solve
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(as.matrix(data),...)
    x$setinverse(im)
    im
}