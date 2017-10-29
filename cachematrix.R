## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix function has 4 functions defined in it, calling these
#functions creates an environment for variables used inside the function
# makeCacheMatrix. This environment save inverse and other function can check if
# we have alredy calculated the inverse there in this environment or not

#it has four functions set,get,setInverse, getInverse and it returns list of
#functions created for argument x which is a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse = matr) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## cache Solve function accepts matrix created through makeCacheMatrix
## It checks if calculated inverse is already there in the enviroment of
## the function
## if not it calculates inverse of the matrix and save in to the above
## functions environment

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    
}


