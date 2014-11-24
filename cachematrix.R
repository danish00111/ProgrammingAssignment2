## Caching the Inverse of a Matrix using makeCacheMatrix and cacheSolve 



## makeCacheMatrix stores the function : set, get, getinverse, setinverse which do the following
##      set : sets the value of the matrix and assign the value of inverse to NULL
##      get : gets the value of the matrix
##      getinverse : gets the inverse of the matrix 
##      setinverse : assigns the value of inverse to the matrix
makeCacheMatrix <- function(Matrix = matrix()) {
        i <- NULL 
        set <- function (y){
                Matrix <<- y
                i <<- NULL 
        }
        
        get <- function () Matrix 
        getinverse <- function () i
        setinverse <- function (Inverse) i <<- Inverse 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve checks weather the inverse of the matrix is stored in makeCacheMatrix.
## if not it computes and assigns the value of inverse

cacheSolve <- function(Matrix, ...) {
        i <- Matrix$getinverse()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        data <- Matrix$get()
        i <- solve(data, ...)
        Matrix$setinverse(i)
        i
}
