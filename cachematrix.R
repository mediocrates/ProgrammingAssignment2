## makeCacheMatrix takes a matrix and returns that matrix
##   along with its inverse, both of which are cached.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(cacheSolve) m <<- cacheSolve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##   cacheSolve takes a makeCacheMatrix object and computes its
##   inverse, by first trying to look up a cached inverse,
##   and if that didn't work, then by computing the
##   inverse explicitly and then storing it.

cacheSolve <- function(x){
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m        
}