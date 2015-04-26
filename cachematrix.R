## There are two functions in this file
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function returns a list to set or get the matrix supplied,and to get or set the inverse for the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(m){
                x<<-m ##Cache the matrix into x
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function checks if the inverse of the matrix is stored in the cache.If yes then it fetches it from cache memory.
##If no then it solves for inverse which is then stored in the cache using setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
         if(!is.null(inv)){
                print("Getting cached data")
                return(inv)
        }
        else{
                print("Solving for inverse")  
                matrix<-x$get()
                inv<-solve(matrix)
                x$setinverse(inv)
                return(inv)
        }
}
