##The  function, makeCacheMatrix creates a special "matrix", 
##which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the reverse of the matrix
##get the value of the reverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL 
      set <- function(y) {
            x <<- y 
            print("in makeCachecMatrix set methid")
            m <<- NULL
      }
      get <- function() x
      setrev <- function(rev) m <<- rev
      getrev <- function() m
      list(set = set, get = get,
           setrev = setrev,
           getrev = getrev)
}

## Function calculates the reverse of the special "matrix" created with the above function. 
## It first checks to see if the reverse has already been calculated. If so, it gets 
## the reverse of the mattrix from the cache and skips the computation. Otherwise, it calculates 
## the reverse of the data and sets the value of the Reverse in the cache via the setrev function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getrev()
      if(!is.null(m)) {
            message("getting cached data")
            return(x$getrev())
      }
      data <- x$get()
      m <- rev(data, ...)
      x$setrev(m)
      m
}

## Function to compute the reverse of the special "matrix" 

rev <- function(matr){
      revmat <- matrix(nrow=ncol(matr), ncol=nrow(matr))
      #print(matr)
      for(i in 1:nrow(matr)){
            for(j in 1:ncol(matr)){
                  if(!is.null(matr[i,j])){
                        revmat[j,i] <- matr[i,j]
                  }
            }
      }
      return(revmat)
}