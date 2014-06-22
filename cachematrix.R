#Homework solution based on vector mean example
#Cache inverse of matrix

#Example usage:
# > m<-1:9
# > m[5]<-11
# > cm<-makeCacheMatrix()  #defines functions
# > cm$set(m)              #sets matrix to CacheMatrix structure
# > cacheSolve(cm)         #calls inverse function and saves in cache
# > cacheSolve(cm)         #returns cached inverse of matrix

# Define functions to set and return a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #define function to store matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #define function to return stored matrix
  setinverse <- function(inverse) m <<- inverse #define function to store inverse
  getinverse <- function() m #define function to return inverse
  list(set = set, get = get, #return list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}

# Return inverse of a matrix, using the cached value if it has already been computed

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #check if inverse is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  #return cached value
  }
  data <- x$get() #get matrix
  m <- solve(data, ...) #compute inverse of matrix
  x$setinverse(m) #cache inverse of matrix
  m #return inverse matrix
}
