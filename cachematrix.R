## Put comments here that give an overall description of what your
## functions do
# Rather than repeatedly inversing a matrix during a loop, there may be 
# benefit of caching the inversed matrix to speed up computing time and saving computing
# resources. Two functions below are used to cache the inverse of a matrix.The functions
# assume the matrix is always invertible.

# The function makeCacheMatrix creates a list with functions to do the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInverseM <- function(inverse) invM <<- inverse
  getInverseM <- function() invM
  list(set = set, get = get,
       setInverseM = setInverseM,
       getInverseM = getInverseM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  invM<-x$getInverseM()
  if(!is.null(invM)) {
    message("getting cached data.")
    return(invM)
  }
  data <- x$get()
  invM<-solve(data)
  x$setInverseM(invM)
  invM
        ## Return a matrix that is the inverse of 'x'
}

## Sample run:
#> x<-rbind(c(1,89), c(89,1))
#> m<-makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1   89
#[2,]   89    1

#Running cacheSolve(m) for the first time it will compute the inverse and cache it
#> cacheSolve(m)
#[,1]          [,2]
#[1,] -0.0001262626  0.0112373737
#[2,]  0.0112373737 -0.0001262626

#running cacheSolve(m) the second time it retrieves the already solved inverse m
#> cacheSolve(m)
#getting cached data.
#[,1]          [,2]
#[1,] -0.0001262626  0.0112373737
#[2,]  0.0112373737 -0.0001262626

