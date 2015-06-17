
makeCacheMatrix <- function(x=matrix()){

	mat <- NULL

	set <- function(y) {
		x <<- y
		mat <- NULL
	}

	get <- function() x

	set_inverse <- function(solve) mat <<- solve

	get_inverse <- function() mat

	list(set=set,
		get=get,
		set_inverse = set_inverse,
		get_inverse = get_inverse)

}



cacheSolve <- function(x, ...) {

##Return a matrix that is the inverse of 'x'

	mat_1 <- x$get_inverse()

	if(!is.null(mat_1)){
		message("Getting cached data")
		return(mat_1)
	}
	
	mat_data <- x$get()
	mat_1 <- solve(mat_data, ...)
	x$set_inverse(mat_1)
	mat_1
}
