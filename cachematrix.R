## Persist the value of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    Minv <- NULL
    getdata <- function() x
    set <- function(y) Minv <<- y
    get <- function() Minv
    list(getdata = getdata, setinv = set, getinv = get)
}


## Calculate or get the value of the inverse of a matrix
cacheSolve <- function(x, ...) {
    Minv <- x$getinv()
    if(!is.null(Minv)) {
        message("getting cached data")
        return(Minv)
    }
    data <- x$getdata()
    Minv <- solve(data)
    x$setinv(Minv)
    Minv
}
