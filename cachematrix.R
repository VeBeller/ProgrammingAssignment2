## The two function below can be used to calculate the inverse of a 2x2 matrix and cache the result for future use/reference

## The function will always first check the cache to see whether the mean has already beeen calculated and stored
## If the mean has already been stored in the cache, the inverse is retrieved from the cache,
## otherwise R will calculate the inverse using solve()

## makeCacheMatrix returns a list of functions to set/get a matrix x, and set/get it's inverse matrix i

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
          x<<-y
          i<<-NULL
        }
        get<-function() x
        setinv<-function(inv) i<<-inv
        getinv<-function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve uses an object created by makeCacheMatrix as input to calculate (or retrieve) the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinv()
        if(!is.null(i)){
          message("getting cached inverse matrix")
          return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinv(i)
        i
}
