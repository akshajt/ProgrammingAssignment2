## Create a special "matrix" that will have a function to 
## cache its inverse

makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse)i<<-inverse
  getinverse <- function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Find the inverse of the matrix created
## If inverse already computed, then the inverse 
## is retrieved from cache

cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m,...)
  x$setinverse(i)
  i
}
