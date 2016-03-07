# How to run this R script
# source("Matrix.r")
# lst_matrix <- makeCacheMatrix()
# m1 <- matrix(c(4,3,3,2), 2, 2)
# lst_matrix$set(m1)
# cacheSolve(lst_matrix) -- calculated inverse
# cacheSolve(lst_matrix) -- get inverse from cache

# Creates a list object containing matrix function
makeCacheMatrix <- function(p_mtx = matrix()) {
	mtx_inv <- NULL
	set <- function(p_mtx1) {
			p_mtx <<- p_mtx1
			mtx_inv <<- NULL
	}
	get <- function() p_mtx
	setMatrixInverse <- function(p_inv) mtx_inv <<- p_inv
	getMatrixInverse <- function() mtx_inv
	list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}

# Calculates matrix inverse and caches it
cacheSolve <- function(p_mtx, ...) {
	inv_matrix <- p_mtx$getMatrixInverse()
	if(!is.null(inv_matrix)) {
			message("getting cached data")
			return(inv_matrix)
	}
	matrix_data <- p_mtx$get()
	inv_matrix <- solve(matrix_data, ...)
	p_mtx$setMatrixInverse(inv_matrix)
	inv_matrix
}
