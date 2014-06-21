## calculate inverse of matrix and save results
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL
        set <- function(y) {
                X <<- y
                inv <<- NULL
        }
        get <- function() X
        setinv <- function(Inv) inv <<- Inv # solve(X)
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
        myInv <- X$getinv()
        if(!is.null(myInv)) {
                message("getting cached data")
                return(myInv)
        }
        data <- X$get()
        myInv <- solve(data, ...)
        X$setinv(myInv)
        myInv
}

# hilbert(8)
# res <- makeCacheMatrix(hilbert(3)); res$get()
# cacheSolve(res)
# res$getinv()
# res$set(hilbert(4))
# cacheSolve(res)
# res$getinv()
