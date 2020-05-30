## makeCacheMatriz is a function tha ask you for a matrix object then its value is stored. The function
## maCacheMatriz does not calcule the inverse, but it creates and object where the result of the inversion is 
##going to be stored. If you want to store a new matry, try " nameoldmatrix$set(namenewmatrix)"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##this new function called "cacheSolve", use the "m" objetc created in the last function to store the inverse result 
##of the stored matrix by using the solve() function. So the result will depend on what matrix it's already stored.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}