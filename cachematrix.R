## Put comments here that give an overall description of what yourfunctions do
## The following two functions together will create a special "matrix" object,compute 
## the inverse of the matrix and cache the inverse of a matrix that can be retrieved in future  
## and save from potentially time-consuming repeated computations.
## 
## 
## In this makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
        x<<-y
        m<<-NULL
        }
        get<-function() x
        setmatrix<-function(matrix) m<<- matrix
        getmatrix<-function() m
        list(set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}


##The cacheSolve function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), the cacheSolve 
##function will retrieve the inverse from the cache, otherwise the function computes the inverse,
##sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
        message("getting cached data")
        return(m)
}
        data<-x$get()
        m<-solve(data)
        x$setmatrix(m)
        m
}   
x = rbind(c(2, -2/4), c(-2/4, 2))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)   ## No cache in this run
cacheSolve(m)   ## Retrieves the inverse of the matrix from the cache in this second run.
