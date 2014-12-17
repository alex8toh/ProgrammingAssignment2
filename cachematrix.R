## This function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  # x cache the input matrix 
  # m cache the inverse matrix 
  m <- NULL
  setMatrix <- function(y)
  {
    # cache the input matrix 
    x <<- y
    
    # initialize the inverse matrix to null
    m <<- NULL
  }
  
  # return the cache input matrix
  getMatrix <- function() x 
  
  # set the inverse matrix 
  setInverseMatrix <- function(inverse) m <<- inverse
  
  # get the inverse matrix 
  getInverseMatrix <- function() m
  
  
  list(set = set, get= get, 
       setInverseMatrix = setInveraseMatrix, 
       getInverseMatrix = getInverseMatrix)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has 
## not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if (!is.null(m))
    {      
      message("getting cache matrix")      
    }
    else
    {
      # run the getMatrix function to get the input matrix 
      y <- x$getMatrix()            
      
      # run the setMatrix function to cache the input matrix 
      X$setMatrix(y)
      
      # comput the inverse of the matrix 
      m <- solve(y, ...)
      
      # cache the inverse matrix 
      x$setInverseMatrix(m)      
    }
    
    # return the inverse matrix 
    return (m)
}
