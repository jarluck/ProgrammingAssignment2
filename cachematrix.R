
## returns wrapper object for matrix w/ fns to set and get inverse.
##Substantially based on rpeng's vector cacheing routines
makeCacheMatrix <- function(x = matrix()) { ##intializes w/ empty matrix if no argument provided
   

  i <- NULL ##placeholder for inverse
  set <- function(y) {
    x <<- y
    i <<- NULL ##changing data makes previous inverse incorrect, so reset
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, ##return list of the sub-functions
       setinverse = setinverse,
       getinverse = getinverse)

}


## return inverse of x. Calculate if not already cached; returns stored value of already there.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) { ##check if inverse already calcuated - if so, return cached value
    message("Getting cached data")
    return(i)
  }
  data <- x$get() 
  i <- solve(data) ##w/o right hand argument, solve() finds inverse
  x$setinverse(i) ##store inverse for future use, then return it
  i
}
