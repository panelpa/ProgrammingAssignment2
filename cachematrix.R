## These two functions enable to cache the inverse of a matrix 


## this function creates a special matrix that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## this function returns the inverse of the special matrix created before
## it will test if the inverse has been already computed


cacheSolve <- function(x, ...) {
	  
	   inv <- x$getInverse()
	  	
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv        


}
