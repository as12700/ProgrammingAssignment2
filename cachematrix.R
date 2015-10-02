##This function will create the inverse of a matrix
# Input Parameter : Matrix
## Return Parameter : Matrix

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



## This function will check if an inverse for a matrix already exists.
## If Inverse exists it does not calculate the inverse. Returns the inverse from cache.
# Input Parameter : Matrix
## Return Parameter : Matrix

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

## This is a small test tools to check how the logic works
## it calcluates the time difference between 1st run & sucessive runs.
# Input Parameter : Matrix
## Return Parameter : Matrix
test
  
testCacheLogic = function(mat){
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
