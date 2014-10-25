## makeCacheMatrix function creates an object to store matrix and its inverse
## cacheSolve function check if inverse exists in makeCacheMatrix type object

## based on Roger D. Peng's 'Caching the Mean of a Vector' code 
## from https://class.coursera.org/rprog-008/human_grading/view/courses/972581/assessments/3/submissions

## new object to store matrix and its inverse

makeCacheMatrix <- function(M = matrix()) {
  I <- NULL # I stends for inverse
  
  set <- function(new.matrix) {
    M <<- new.matrix
    I <<- NULL
  }
  
  get <- function() M
  
  setinverse <- function(matrix.inverse) I <<- matrix.inverse
  
  getinverse <- function() I
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this one gets inverse in one does not exist or extract one from cache

cacheSolve <- function(M) {
  
  I <- M$getinverse() 
  
  if(!is.null(I)) {
  
    message("getting inverse from cache")
    return(I)
  
  } else {
    
    data <- M$get()
    I <- solve(data)
    M$setinverse(I)
    return(I)
  }
  
}
