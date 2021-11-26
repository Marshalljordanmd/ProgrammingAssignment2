## These functions work together to cache a computed
## matrix inverse so that it can be retrieved repeatedly
## for use in a global enviroment different from the 
## enviroment where the matrix inverse was originally computed. 



## "makeCacheMatrix" takes a square matrix as an argument
## and returns a list of functions to change the matrix ('set'),
## get the matrix ('get'), set the inverse ('setinv'), or get the 
## inverse ('getinv')

makeCacheMatrix <- function(X=matrix()) {
    inv <- NULL
    set <- function(Y=matrix()) {
        X <<- Y
        inv <<- NULL
    }
    get <- function() X
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, 
         getinv = getinv)
}


## "cacheSolve" takes the "makeCacheMatrix" function as an argument and 
## either returns the cached matrix inverse, or computes and caches the inverse 
## and then returns it.

cacheSolve <- function(f,...){
    inv <- f$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- f$get()
    inv <- solve(data)
    f$setinv(inv)
    inv
}
