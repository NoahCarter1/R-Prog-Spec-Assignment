
##This only works when called with the first matrix called: if you subsequently use cacheSolve(makeCacheMatrix()) with a different matrix arg, there will be an error
##The comments are confused
##this function creates a matrix object that can cache its inverse
#this function simply returns a list that contains four functions. These functions can be calling their name using name
makeCacheMatrix<-function(x=matrix())
{
  m <- NULL
  #I'm not sure why the creation of this function was necessary, since set is never called
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x #returns x (the original matrix), if no arg was passed for makeCacheMatrix, returns empty matrix
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}#end makeCacheMatrix()

##this function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.
#this function accepts a list of functions, with which performs operations
cacheSolve<-function(x,...)##here, x represents a list of functions, not the original matrix
{
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}#end cacheSolve()