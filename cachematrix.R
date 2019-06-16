#makeCacheMatrix makes a special matrix of the argument to have
# certain functions
makeCacheMatrix<- function (x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInverse<-function(Inverse) i<<-Inverse
  getInverse<-function() Inverse
  list (Set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
#cacheSolve function calculates the inverse of a matrix
# by first checking if there is already a pre-existing
# inverse. If not, it calculates the inverse and sets it
cacheSolve<-function(x,...){
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setInverse(i)
}
