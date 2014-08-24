## This function will make a matrix object that can
## cache it's inverse


## ## This function will make a matrix object that can
## cache it's inverse

makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setmatrix <- function (solve) m <<- solve
	getmatrix <- function () m
	list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## This function retrieves a matrix's cached inverse if it exists,
## otherwise, it will compute a matrix's inverse
cacheSolve <- function(x = matrix(), ...){
	m <- x$getmatrix()
	if(!is.null(m)){
		print("getting cached data")
		return (m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}
