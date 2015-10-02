## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  
    inv <- NULL 
    set <- function(y) { 
      x <<- y 
      inv <<- NULL 
    } 
    get <- function() x 
      setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
  } 



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  
    inv <- x$getinverse() 
    if(!is.null(inv)) { 
      message("getting cached data.") 
      return(inv) 
    } 
    
    data <- x$get() 
    inv <- solve(data) 
    x$setinverse(inv) 
    inv 
  } 
  
  
test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}
