## Put comments here that give an overall description of what your
## functions do

## creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  # stores inverse in m
  m <- NULL
  Matrix <- x
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() Matrix
  
  getinverse <- function() m
  
  ## set the inverse
  setsolution <- function(soln) m <<- soln
  
  ## returns list of the functions defined
  list(
    set=set,
    get = get,
    setsolution = setsolution,
    getinverse = getinverse
  )
}

## returns the inverse of a matrix if already cached else solves the matrix
cacheSolve <- function(x, ...){
  # get inverse if not null then return inverse
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## get the Matrix from function call so that it can be passed to solve()
  Matrix <- x$get()
  m <- solve(Matrix, ...)
  x$setsolution(m)
  ## return inverse
  m
}
