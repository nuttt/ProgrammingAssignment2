## cachematrix.R is a special matrix object
## that can compute, then cache it's inverse in order to reduce computational cost

## create a special inverse-cached matrix
## contains 4 methods: set get setInverse getInverse

makeCacheMatrix <- function(x = matrix()) {
	
	x.inverse <- NULL
	
	set <- function(x.new) {
		x <<- x.new
		x.inverse <<- NULL
	}
	
	get <- function() {
		x
	}
	
	setInverse <- function(inverse) {
		x.inverse <<- inverse
	}
	
	getInverse <- function() {
		x.inverse
	}
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## compute inverse of cachematrix
## if matrix's inverse have been computed before
## it will return the answer from it's cache

cacheSolve <- function(x, ...) {
	
	## Lazy calculation of x's inverse
	if ( is.null( x$getInverse() ) ) {
		x$setInverse( solve(x$get()) )
	}
	
	x$getInverse()
}
