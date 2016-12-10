## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function -- OK

## The 'makeCacheMatrix' function creates a special "matrix" object 
## that can cache its inverse. Per the assignment instructions, we 
## are to assume matrix supplied is always invertible and we can 
## use the 'solve' function to calculate the inverse of a matrix. 
## In general, the function receives a matrix as input and creates 
## a special "matrix" object that for storing the inverse of the
## provided matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    inv_matrix <- NULL
    
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(solve) inv_matrix <<- solve
    
    get_inverse <- function() inv_matrix
    
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function -- OK

## The 'cacheSolve' function checks to to see if the inverse
## of the provided special "matrix" object, created using the
## 'makeCachedMatrix' function above, has already been
## calclulated. If it has been previously calculated, it will
## get the inverse from cache and skip the computation. If
## the inverse has not been previously calculated, it will 
## calculate the inverse and set the value of the inverse in
## the cache via the 'set_inverse' function.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m_inverse <- x$get_inverse()
    
    if(!is.null(m_inverse)) {
        
        message("getting cached matrix")
        
        return(m_inverse)
    }
    
    data <- x$get()
    
    m_inverse <- solve(data, ...)
    
    x$set_inverse(m_inverse)
    
    m_inverse
}
