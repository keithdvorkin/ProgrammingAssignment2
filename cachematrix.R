## This function creates a special "matrix" object
## that can cache its inverse.  It is modeled off of
## the makeVector example provided in the assignment
## description.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
  ##print(x)

}


# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# It is again modeled off of the cachemean function provided in 
# the example

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
      }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
