## Programming assignment to create a simple cache function that wraps finding 
## the inverse of a matrix
##  Uses R solve function to find the inverse matrix
##
## Matt Licholai (mattlic@outlook.com)  
##      April 2014


## makeCacheMatrix: This function creates a special "matrix" object 
##   that can cache its inverse.

makeCacheMatrix <- function(xMatrixX = matrix()) {
    
    inverseMatrix <- NULL
    set <- function(y) {
        xMatrixX <<- y
        inverseMatrix <<- NULL
    }
    get <- function() xMatrixX
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    

}


## cacheSolve: This function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been 
##   calculated (and the matrix has not changed), then the cachesolve  
##   retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse    
}


## simple unit tests
testCacheSolve <- function() {
    hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
    h8 <- hilbert(8); h8
    sh8 = solve(h8)
    
    ch8 <- makeCacheMatrix( h8 )
    sch8 <- cacheSolve( ch8 )
    
    if (sum(round(sh8 %*% h8, 3) == round(sch8 %*% h8)) == length(h8)) {
        print("Simple test 1 .. OK" )
    }
    else {
        Print("FAILED: Simple test 1 !!!")
    }
    
    print(" ")
    print("should see message ... 'getting cached data'")
    if ( sum(round(sh8 %*% h8, 3) == round(cacheSolve( ch8 ) %*% h8)) == length(h8)) {
        print("Simple test 2 .. OK")
    }
    else{
        Print("FAILED: Simple test 2 !!!")
    }
    
    print(" ")
    print("should see message ... 'getting cached data'")
    if ( sum(round(sh8 %*% h8, 3) == round(cacheSolve( ch8 ) %*% ch8$get())) == length(h8)) {
        print("Simple test 3 .. OK")
    }
    else{
        Print("FAILED: Simple test 3 !!!")
    }
    
    print(" ")
    print("using cache, but should not see a message ... ")
    if ( sum(round(sh8 %*% h8, 3) == round( ch8$getInverse() %*% ch8$get())) == length(h8)) {
        print("Simple test 4 .. OK")
    }
    else{
        Print("FAILED: Simple test 4 !!!")
    }
    
}
