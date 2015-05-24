## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
    i <- NULL
    ###when i<- NULL is deleted, if we run y after x, cacheSolve###
    ###will return the inverse of x###
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    ###use << in 'set*'function enable to reuse it in 'get*'function###
}


## Write a short comment describing this function

cacheSolve <- function(x,...)
{
    ###when inverse exist, no need to recalculate, save time###
    inverse <- x$getinverse()
    if(!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    
    ###when not inverse exist, calculate###
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}

        ## Return a matrix that is the inverse of 'x'

