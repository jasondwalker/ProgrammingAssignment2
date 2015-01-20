## For Programming assignment 2, R Programming, Coursera.
## A pair of functions. The first caches the inverse of a matrix, the second either retrieves
## this cache (if available) or solves the inverse if not.


## 1) Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL           #create empty variable
        set <- function(x) {
                x <<- y       #in Global Environment
                inv <<- NULL  #in Global Environment
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve  #in Global Environment
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv=getinv)
}


## 2) Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- getinv()     #retrieve inverse
        xold <- get()         #retrieve original matrix
        if(!is.null(inv)      #check inverse cached
           &&
        identical(x,xold)) {  #check matrix unchanged
                message("getting cached data")
                return(inv)
        }
        data <- get()       #otherwise calculate inverse
        inv <- solve(data, ...)
        setinv(inv)
        inv
}
