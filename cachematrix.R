## Put comments here that give an overall description of what your
## functions do

#------------------
# Programming Assignment 2
#
# (*) The following functions enable the creation of a "cached" matrix.
#     Specifically, it creates a strcuture where the inverse of a matrix
#     is calculated once and stored. It is not re-calculated multiple times
#     in order to speed things up.
# (*) However, when the matrix has changed (or a new one has been created),
#     the inverse is recalculated again.
#------------------


## Write a short comment describing this function
#------------------
# (*) The makeCacheMatrix function creates a special matrix data structure
#     similar to the vector structure in the makeVector example.
# (*) Specifically, it creates a list of functions to set and get the matrix
#     and a set/get function pair for the inverse of the matrix.
#------------------

makeCacheMatrix <- function(x = matrix()) {

    # When creating a new matrix using the makeCacheMatrix function
    # set the inverse to NULL
    inv <- NULL
    
    # set: constructor function for a new matrix that does 2 things:
    # 1) sets the value of a new matrix
    # 2) Resets the inv to NULL so that it will be recalculated
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get: return the matrix
    get <- function() x
    
    # set the inverse of the matrix by passing the inverse
    setinv <- function(inverse) inv <<- inverse
    
    # get the inverse of the matrix
    getinv <- function() inv
    
    # return value of makeCacheMatrix:
    # a list of 4 functions: set, get, setinv, getinv
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
    
}


## Write a short comment describing this function
#------------------
# (*) The cacheSolve function calculates the inverse of a matrix
#     only when the inverse has not already been calculated.
# (*) This utilizes the structure created in the makeCacheMatrix
#     function above.
# (*) It is assumed that the input matrix is always invertible
# (*) Allow for a ... operator to pass extra arguments to solve()
#     as needed (e.g. LINPACK)
#------------------
cacheSolve <- function(x, ...) {
    
    # get the inverse using the getinv() function
    # ---------    
    inv <- x$getinv()
    
    # check if the inverse already exists
    #   if yes, simply return the inverse, do not recalculate
    #   and exit the function
    # ---------
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # if the inverse is NULL (does not exist)
    #   1) get the data
    #   2) calculate the inverse
    #   3) set the inverse (so you dont have to recalculate next time around)
    #   4) return the inverse
    # ---------    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinv(inv)
    
    inv
    
}


