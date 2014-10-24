## Put comments here that give an overall description of what your
## functions do


    ## These two functions are used to cache the inverse of a matrix
    ##    so we don't have to compute the inverse of the same matrix more than once

    ## First we create a matrix and use it as the argument to "makeCacheMatrix()"

    ## Second we use "cacheSolve()" to compute the inverse of the matrix,
    ##    cache this inverse, and return this inverse

    ## If 'cacheSolve()" is called again, it checks to see if the  matrix inverse has
    ##    been calculated before and if so it returns the inverse from its cache

    ## Here is an example:
        ##  > amatrix = makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))

        ##  > amatrix$get()            # returns original matrix
        ##       [,1] [,2]
        ##  [1,]    1    3
        ##  [2,]    2    4

        ##  > cacheSolve(amatrix)      # computes, caches, and returns
        ##       [,1] [,2]             # matrix inverse
        ##  [1,]   -2  1.5
        ##  [2,]    1 -0.5

        ##  > amatrix$getsolve()       # returns matrix inverse
        ##       [,1] [,2]
        ##  [1,]   -2  1.5
        ##  [2,]    1 -0.5

        ##  > cacheSolve(amatrix)      # returns cached matrix inverse
        ##  getting cached data
        ##       [,1] [,2]
        ##  [1,]   -2  1.5
        ##  [2,]    1 -0.5
        ##  > 

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to

    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the inverse of the matrix
    ## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" created
## with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

