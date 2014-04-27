## Functions below efficiently calculate and cache matrix inverses

## Caching only occurs once for new matrices
## Retrieval occurs for old matrices instead of a costly computation

## To use this pair of functions:
## First initialize the cache with cp <- makeCacheMatrix(p) where p is a square matrix
## Then run                      pinv <- cacheSolve(cp)     to retrieve/calculate the matrix inverse as needed

## Note that the functions assume that the matrix is invertible --- i.e., square, nonsingular
## If not, solve aborts ungracefully with an error message

makeCacheMatrix <- function(x = matrix()) {                                         
    
    ## Returns a matrix object of four elements
    
    ## the first two are functions which get/set the value of the matrix
    ## the second two are functions that setinv/getinv the values of its inverse 
    
    ## NOTE the reversal in order of getting/setting
    
    minv <- NULL
    set <- function(y){
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
      
    ## Returns the matrix inverse of 'x'
    
    ## First it checks to see if the inverse has already been calculated
    ## If so, it retrieves the calculated value
    ## Else it calculates the inverse and caches it for later use
    
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("retrieving cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}
