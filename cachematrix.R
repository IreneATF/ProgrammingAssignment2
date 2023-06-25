## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      if(ncol(x)==nrow(x) && det(x)!=0){
            inve <- NULL
            set <- function(y) {
                  x <<- y
                  inve <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inve <<- inverse
            getinverse <- function() inve
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
      }else{return(message("The matrix is not invertible."))}
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      inve <- x$getinverse()
      if(!is.null(inve)) {
            message("getting cached data")
            return(inve)
      }
      data <- x$get()
      inve <- solve(data, ...)  ## calculating inverse
      x$setinverse(inve)
      inve
       
}

setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass2/ProgrammingAssignment2")
source("cachematrix.R")

## Testing the function with StackOverflow example
x <-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
cacheSolve(x)
      #[,1] [,2] [,3]
#[1,]    1    0  0.0
#[2,]    0    1  0.0
#[3,]    0    0  0.5
x$get()
      #[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    2
x$getinverse()
      #[,1] [,2] [,3]
#[1,]    1    0  0.0
#[2,]    0    1  0.0
#[3,]    0    0  0.5