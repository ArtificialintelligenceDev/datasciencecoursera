# Joseph Santos 8-5-2018 [JHU]


# This fxn creates a special "matrix" containing a fxn to:  
# set val of matrix  (Set)
# get val of matrix  (Find)
# set val of inverse (setInv)
# get val of inverse (getInv)

makeCacheMatrix <- function(m = matrix()) {
  # n means matrix value == NULL.
  n <- NULL
  set <- function(y){
    # Assign m to value in diff. env.
    m <<- y
    # Assign n to value in diff. env.
    n <<- NULL
  }
  # Get the value of the matrix(m)
  getVal <- function() m
  # Set the inverse value of the matrix
  setInv <- function(inverse) 
    n <<- inverse
  #Retrieve vals.
  getInv <- function() 
    n
  # Create the special "matrix".
  list(Set = Set, getVal = getVal, setInv = setInv, getInv = getInv)

}


## This fxn finds inverse of the "matrix" returned by makeCacheMatrix.

cacheSolve <- function(m, ...) {
  # IF n is NOT NULL, return n and let user know.
  invMatrix <- m$getInv()
  if (!is.null(n)) {
    # UX for the user.
    message("Retrieving the inverse from the cache!")
    return(n)
  }
  # Assign val to dat.
  dat <- m$get()
  # Compute the inverse of the square matrix.
  n <- solve(dat,...)
  m$setinverse(n)
  n
}
  

