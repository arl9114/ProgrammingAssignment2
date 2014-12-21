## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        z<-NULL
        set <- function(y) {
          x <<- y
          z <<- NULL
        }
        get <- function() x
        setinverse<-function(m){
          z<<-(m)
        }
        getinverse<-function(){
          z
        }
        list(set=set,get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
