# This function takes in a matrix and performs the following functions:
# 1. sets the matrix
# 2. gets the matrix
# 3. sets the inverse
# 4. gets the inverse
# It is extremely similar to the makeVector function provided in the 
# example, except that this is done to a matrix instead of a vector. 

makeCacheMatrix <- function(obj) {
  
  #This if statement eliminates all non-matrix objects from consideration,
  #as well as any non-square matrices. We assume that the matrices that
  #passed the filter are invertible for this assignment.
  if(class(obj) != "matrix") {
    print("Not a matrix")
  } else if (nrow(obj) != ncol(obj)) {
    print("Matrix not invertible")
  } else {
    solution <- NULL;
    
    #Caches the original matrix
    set <- function (mat) {
      obj <<- mat
      solution <<- NULL
    }
    
    #Gets the cached matrix 
    get <- function() obj
    
    #The last two functions make it possible to set and get the inverses
    #to the original matrix.
    setinv <- function(inverse) solution <<- inverse
    getinv <- function() solution
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  }
}