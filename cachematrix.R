## Caching the inverse of a matrix by MakeCacheMatrix and CacheSolve functions


## MakeCacheMatrix function is creating a matrix that can cache the inverse for a input

makeCacheMatrix <- function(x = matrix()) {
        
        inversion <- NULL
                set <- function(y) {
                        x <<- y
                        inversion <<- NULL
                }
                
        get <- function() x
        setinv <- function(inverse) inversion <<- inverse
        getinv <- function() inversion
        
        list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## CacheSolve function is computing the matrix inversion 

cacheSolve <- function(x= matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        inversion <- x$getinv()
                if(!is.null(inversion)) {
                        message("getting cached data")
                        return(inversion)
                }
        
        data <- x$get()
        inversion <- solve(data, ...)
        x$setinv(inversion)
        inversion
        
       
}
