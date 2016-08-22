## This script has two functions and some tests at the end. The first function creates special matrix objects that can hold and manage cached data, the second one is responsible for computing the inverse (if not yet cached) for one such special matrix and finally I have some tests for a large

## Cleaning up workspace before starting
rm(list=ls())

## This function creates an CacheMatrix object which contains the data for the input matrix x and provides a set of methods to access or assign the matrix x data and the cached inverse of the matrix x
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  # function to clear cached value and reset it to a new value y
  set <- function(y) {
    x <<- y #new value for the matrix
    invx <<- NULL # clears the inverse so that it has to be recalculated
  }
  # function to get the value of the matrix
  get <- function()x
  # funtion to set the value of the inverse according to some input calculation
  setinv <- function(inv) invx<<-inv
  #function to get the value of the inverse that is stored in the object
  getinv <- function() invx
  # finally return a list with all the methods
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## This function computes the inverse of the input matrix of type CacheMatrix (see function above) if not already cached

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("Getting the cached data")
    return(inv) 
  }
  inv <- solve(x$get(),...)
  x$setinv(inv)
  inv
}

# Let us test with a very large matrix to see the difference in speed
## Performing the first computation of the inverse
nsize <- 1500
mat <- matrix(rnorm(nsize*nsize),nrow=nsize) # assigning a matrix
inv_mat<- solve(mat) # saving what qwe are supposed to get

## Create special matrix now
mat_cached <- makeCacheMatrix(mat) 
mat_cached$getinv()

### Computing inverse and chaching value (first call to calculate it)
sum(cacheSolve(mat_cached)-inv_mat)

### second call to check that the cached version is being used (it is much faster)
sum(cacheSolve(mat_cached)-inv_mat)

### check that the cached data can be obtained directly from the special matrix
sum(mat_cached$getinv()-inv_mat)
