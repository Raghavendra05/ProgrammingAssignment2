## The below function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inve = NULL
  set = function(y){
    x <<-y
    inv <<- NULL
  }
  get =function() x
  setinve = function(inverse) inve <<- inverse
  getinve = function() inve
  list(set=set,get=get,setinve=setinve,getinve=getinve)

}


## This function computes the inverse of the matrix returned by above function.
## If the inverse has already been calculated and the matrix has not changed,
## then the cachesolve should retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inve = x$getinve()
  
  if(!is.null(inve)){
    
    message("retrieve the inverse from the cache")
    return(inve)
  }
  
  mat.data =x$get()
  inve =solve(mat.data,...)
  x$setinve(inve)
  return(inve)
}
