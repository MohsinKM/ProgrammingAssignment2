## This code is written by KM Mohsin (mohsin.eee@gmail.com) on 6/20/2016
## First function is to create a matrix object to calculate and cache inverse matrix
##Second function is actually where we are using the defined functions closure in the 
# first function. 

##usages: R> x<-makeCacheMatrix(mat1) #mat1 is an invertible matrix
##R> cacheSolve(x) #this will either return data, or calculate, cache and then return. 

makeCacheMatrix <- function(x = matrix()) {
    inv_mat<- matrix(data=NA,nrow(x),ncol(x))       #initializing a null matrix for inv
    set <- function(y) {
        x <<- y
        inv_mat <<- matrix(data=NA,nrow(x),ncol(x))
    }
    get <- function() x                             #functioin to retreive matrix
    setinv <- function(inverse) inv_mat <<- inverse #function to cache inverse matrix
    getinv <- function() inv_mat                    #func to get the cached inverse matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)                           #listing all functions, these 4 will be 
                                                    #returned after calling this function. 
}


## Second one checks whether inverese matrix is calculated previously or 
#not. if yes then it return cached result. if not then it calculates, cache and return the 
#restult. Here result is inverse of a matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$getinv()                           #retrieving inverse matrix
    if(!all(is.na(inv_mat))) {                      
        message("getting cached data")
        return(inv_mat)                             #checking whether cached or not. 
    }
    data <- x$get()                                 #Bringing matrix for calculation
    inv_mat <- solve(data, ...)                     #inverting
    x$setinv(inv_mat)                               #caching
    inv_mat                                         #returning inverse matrix
}
