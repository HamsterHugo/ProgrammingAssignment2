## The two functions makeCacheMatrix and cacheSolve compute and cache the inverse of a quadratic matrix
## assuming the matrix is invertible.

## The function makeCacheMatrix creates a special "matrix" object. The function gets a quadratic invertible
## matrix as parameter. It returns a list of four internal functions set, get, setInverse and getInverse.
## Get and getInverse return the matrix or the inverse of the matrix, respectively. Set and setInverse
## set the attributes x and inv, where x is the matrix given as parameter to the function and inv is its inverse.
## Notice that inv is set NULL by default. The inverse of x is computed in the function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<-y ## The <<- assignment operator is used to change x in the parent environment.
                inv<<-NULL ## Reset the inv variable when the matrix x is changed (in the parent environment).
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## The function cacheSolve gets a matrix object created by the function makeCacheMatrix. Then it checks if the inverse
## of x is already in the cache. If not, it computes the inverse, chaches and returns it. If the inverse matrix is already
## in the cache, it does not compute the inverse again. Instead it return the cached inverse matrix via the getInverse function
## of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getInverse() ## The getInverse function of the makeCacheMatrix function is used.
        if(!is.null(inverse)){ ## Checks if the inverse is already in the cache.
                message("Getting cache data:") ## Note to the user that the inverse is in the cache.
                return(inverse) ## Returning the already computed inverse.
        }
        data<-x$get()
        inverse<-solve(data) ## Computing the inverse of the matrix.
        x$setInverse(inverse)
        inverse ## Returning the computed inverse.
}
