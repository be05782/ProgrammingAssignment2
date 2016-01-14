## The function makeCacheMatrix creates a list containing a function that

##set the value of the matrix
##get the value of the matrix
##set the inverse matrix (just because it was in the makeVector but it is useless)
##get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
     set <- function(y) {
        x <<- y
        i <<- NULL
   }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## this function will inverse the matrix and store the result in the cache
## if it is not already stored in the cache. Otherwise it will simply get it
## from the cache.
##example
## mdat<-matrix(c(1,2,3,4),nrow=2,ncol=2)
## a<-makeCacheMatrix(mdat)
##CacheSolve(a)
##       [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x, ...) {
       
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
