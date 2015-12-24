
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## get the value of the matrix
## set the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}
    

## if the inverse matrix has already calculated, it will return the cache data
## elsa it will calculate the inverse matrix and store it in cache via the setinverse function
        
cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
        