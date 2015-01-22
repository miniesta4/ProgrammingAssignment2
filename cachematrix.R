## miniesta4

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <-NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and not changed), retrieve it from the cache
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setInverse(i)
	i
}
