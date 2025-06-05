## The function takes a matrix, x, then locally sets inv(inverse of matrix) to NULL
## The function then sets x to be a new matrix then make inv NULL in parent env where it's called
## basic get function just returns the new_matrix
## setinv sets inv to whatever is the new inverse
## getinv retrieves this new inv
## list(set = set, get = get, setinv = setinv, getinv = getinv) looks like a constructor for class
## why in cacheSolve you can go like x$get() I would think

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(new_matrix){
    x <<- new_matrix
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(new_inv) inv <<- new_inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## get inverse see if this inverse actually exists, if it does simply return it
## This should work because of lexical scoping 
## which means that x is in the environment where this function is called
## if does not exist then we get matrix solve for inverse
## Then make sure to setinv which remember sets it with <<- in parent environment
## then just return inv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
