## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invrs<-NULL
  
  set<-function(t){
    x<<-t
    invrs<<-NULL
  }
  
  get<-function() x
  
  setInvrs<-function(y) invrs<<-y
  
  getInvrs<-function() invrs
  
  list(set = set, get = get,
       setInvrs = setInvrs,
       getInvrs = getInvrs)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  
  data <- x$get()
  
  if(nrow(data)==ncol(data))
  {
    invrs=solve(data)
    x$setInvrs(invrs)
    
  }
  else
  {
    library("MASS")
    invrs=ginv(data)
    x$setInvrs(invrs)
  }
  invrs
}
