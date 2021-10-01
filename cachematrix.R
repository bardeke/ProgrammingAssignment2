## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# returns inverse of a matrix
# if is already there, it returns it
# if not, it gets calculated and then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getinverse()
        if(!is.null(iv)) {
                message("getting cached data.")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data,...)
        x$setinverse(iv)
        iv
}
