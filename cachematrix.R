##The first function, `makecachematrix` creates a special "matrix", 

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of matrix
##4.  get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if the
##inverse has already been calculated.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get
  
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}