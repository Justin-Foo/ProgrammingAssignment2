## This function serves to cache the inverse of a matrix.

## The first function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            # uses `<<-` to assign a value to an object in an environment different from the current environment
            x<<-y
            m<<-NULL
      }
      get <- function()x
      setsolve<-function(solve)m<<-
            solve
      getsolve<-function()m
      list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
      
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
      m<-x$getsolve()
      # below skips the computation if the result is already stored in the cache
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      # if result not in cache, it will computate the inverse and return the result in m
      data<-x$get()
      m<-solve(data,...)
      x$setsolve(m)
      m
}
