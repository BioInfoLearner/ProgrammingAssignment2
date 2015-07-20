## This function will creat a special matrix object that can cache its inverse
## It returns a a list containing 4 functios
## The argument of the function is a matrix.
## The first function 'set' will set the value of the matrix.
## The second function 'get' will get the value of the matrix.
## The third function 'set_inv_matrix' will compute the inverse of the matrix.
## The fourth function 'get_inv_matrix' will get the value of the inverse of
## the matrix.
makeCacheMatrix <- function(x = matrix()) {
        # create a variable s with value NULL
        s <- NULL
        # creat the function 'set' 
        set <- function(y) {
                # assign value y to matrix x
                x <<- y
                # assign value Null to variable s
                s <<- NULL
        }
        # creat the function 'get' which will return the matrix x
        get <- function() x
        # creat the function 'set_inv_matrix' which will compute the
        # inverse of the matrix
        set_inv_matrix <- function(solve) s <<- solve
        # creat the function 'get_inv_matrix' which will return the
        # inverse of the matrix
        get_inv_matrix <- function() s
        # return a vector of the 4 functions
        list(set = set, get = get,
             set_inv_matrix = set_inv-matrix,
             get_inv_matrix = get_inv_matrix)
}


## This function computes the inverse of a matrix, returned by 
## makeCacheMatrix above. Check if it has been calculated. 
## If so, it will retieve the inverse from the cache. Otherwise, 
## it caculate the inverse of the matrix and set the inverse 
## matrix in the cache.
cacheSolve <- function(x, ...) {
        # compute the inverse of matrix x
        s <- x$get_inv_matrix()
        # check if it has been calculated.
        # If so, retrive the inverse from the cache and
        # return it.
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        # If the inverse matrix is not in the cache, calculate
        # the inverse of the matrix
        data <- x$get()
        s <- solve(data, ...)
        # set the inverse of the matrix in the cache
        x$set_inv_matrix(s)
        ## Return a matrix that is the inverse of x
        s
}