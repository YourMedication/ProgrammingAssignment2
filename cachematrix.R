## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
	## Set the value of the Matrix

	setMatrix <- function(y) { 
	
		x <<- y
		i <<- NULL
	}
	## Get the value of the matrix

	getMatrix <- function() x 

	## Set the value of the inverse

	setInverse <- function(inverse) i <<- inverse 
	
	## Get the value of the inverse

	getInverse <- function() i 

	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}


## This function returns the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

i <- x$getInverse()

	if(!is.null(i)) {
		message("getting cached data.")
		return(i)
	}
	
	## Solves for the inverse if it is not already cached.

	data <- x$getMatrix()
	i <- solve(data)
	x$setInverse(i)
	i

}
