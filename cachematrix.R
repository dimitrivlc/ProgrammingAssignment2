##makeCacheMatrix is meant to create a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix inverse
#set the value of the matrix inverse
#get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(y) {
                x <<- y
                matrix_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matrix_inv <<- solve
        getinv <- function() matrix_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. H
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inv <- x$getinv()
        if(!is.null(matrix_inv)) {
                message("getting cached data")
                return(matrix_inv)
        }
        data <- x$get()
        matrix_inv <- solve(data, ...)
        x$setinv(matrix_inv)
        matrix_inv
}


