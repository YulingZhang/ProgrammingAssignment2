## This is a function that can be cached inverse matrix of right.

## This function can be used to create a special cache inverse matrix of "Matrix" object.

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) s<<-solve
        getsolve<-function() s
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## This function is used to calculate special "matrix" inverse matrix above makeCacheMatrix returned. 
## If you have to calculate the inverse matrix (and have not changed the matrix),
## then the cache will retrieve cachesolve inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsolve()
        if(!is.null(s)){
                message("getting cache data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setsolve(s)
        s
}
