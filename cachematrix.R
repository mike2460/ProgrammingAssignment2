##  Create two functions  

##  This first function creates a special "matrix" object that can cache its inverse.
##  We define 4 subfunctions:
##  set - sets the value of the matrix 
##  get - returns the value of the matrix
##  set_inverse - calculates and sets the value of the inverse of the matrix
##  get_inverse - returns the value of the inverted matrix


makeCacheMatrix <- function(x=matrix()) {

	mat <- NULL

	set <- function(y) {
		x 	<<- y
		mat <<- NULL
	}

	get <- function() x

	set_inverse <- function(solve) mat <<- solve

	get_inverse <- function() mat

	list(set=set,
		get=get,
		set_inverse = set_inverse,
		get_inverse = get_inverse
		)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

##  Get the matrix object via the get_inverse function of makeCacheMatrix

	mat_1 <- x$get_inverse()

##  Check for the existence of a cached value and return it if found.

	if(!is.null(mat_1)) {
		message("Getting cached data")
		return(mat_1)
	}
	
##  Otherwise, calculate the inverse and set the value in the matrix object, then return the inverse.

	mat_data <- x$get()
	mat_1 <- solve(mat_data, ...)
	x$set_inverse(mat_1)
	mat_1
}
