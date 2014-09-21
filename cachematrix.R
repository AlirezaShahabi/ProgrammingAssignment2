## This program calculates the inverse of a matrix. If the inverse is supposed to
## be calculated many times and the content of the matrix are not changing, its 
## better to cache the value of inverse and re-use that instead of calculating the
## inverse every time.


## This function generates the cache to matrix and its inverse. It returns a list
## where the elements of the list are refrences to the following functions:
## set:         sest a value to the matrix
## get:         returns the valeue of the matrix
## setinverse:  sets a value to the inverse of the matrix
## getinverse:  returns the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        inv <<- NULL
        x   <<- y
    }
    get<-function() x
    setinverse<-function(inverse){inv <<- inverse}
    getinverse<-function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 
}


## This function gets the special matrix produced by makeCacheMatrix function as 
## input. It checks whether the inverse is already calculated. Then follows one of
## thse two path:
## if inverse is calculated:  it returns the value of the inverse
## otherwise: it calculates the inverse using solve function and returns the result

cacheSolve <- function(x, ...) {
    inv<-x$getinverse()
    if (!is.null(inv)){
        message("getting the cashed data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinverse(inv)
    inv
}
