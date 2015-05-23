# The point of this function is to take a matrix and invert it,
# returning the result. It is like cacheMean in the example, except 
# we're doing it to a matrix instead of a vector.

cacheSolve <- function (m, ...) {
  inverse <- m$getinv()
  
  #If we've already solved the problem, then we should look inside the
  #cache for stored answers, instead of calculating all over again.
  if(!is.null(inverse)){
    message("getting cache")
    inverse
    
  #If we don't have the answer yet, we'll calculate the inverse first.
  } else {
    data <- m$get()
    inverse <- solve(data,...)
    m$setinv(inverse)
    inverse
  }
}