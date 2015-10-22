## The functions here demonstrate how to cache the inverse of a matrix
## with the assumption that the matrix given is a square matrix while
## also assuming that a result is produced provided the matrix is invertible


## This function sets the matrix and also constructs sub-functions that allow 
## the user to call the inverse of the matrix from the cache. 

## It also builds functions that allows the user to set the inverse of the matrix
## but this is not advisable as it will break the function.

## The setinverse sub-function is created such that it can be invoked from the
## cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        list(set=set, get=get,
             setinverse=setinverse, 
             getinverse=getinverse)
}


## This function first searches to see if the inverse of the matrix has been cached
## If the matrix is cached then the function does not need to compute a second time
## Otherwise, the function will solve for the inverse of the matrix and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Retrieving values from the cache. If cache is not null, the value of m
        ## is returned without further computation
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
