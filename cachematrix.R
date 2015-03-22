## This function creates a list which has four functions
## 1. set the value of the "matrix"
## 2. get the value of the "matrix"
## 3. set the value of the Inverse of the "matrix"
## 4. get the value of the Inverse of the "matrix"
## It uses the <<- operator which allows assignment of 
## these variables in a different enviroment than the current one.

## This function creates a special "matrix" which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	mtrxInv <- NULL
	
	set <- function(y) {
		x <<- y
		mtrxInv <<- NULL 
	}

	get <- function() x
	setInv <- function(invMatrix) mtrxInv <<- invMatrix
	getInv <- function() mtrxInv

	list(set = set, get = get, setInv = setInv, getInv = getInv)
	
}


## This function calculates the inverse of the special "matrix" above. 
## If the inverse has already been calculated, it retrieves from cache.

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'

	m <- x$getInv()
	
	if(!is.null(m)) {
		message("Retrieving data from cache")
		return(m)
	}

	data <-x$get()
	m <- solve(data)
	x$setInv(m)
	m

}
