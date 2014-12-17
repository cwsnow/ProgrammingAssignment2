## makeCatchMatrix and cacheSolve create the inverse of a matrix(x) and catch the result
## on subsequent calls the cache of the inverese matrix will be returned. This saves
## computing time on resource heavy computations.

# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.

makeCatchMatrix <- function(x = matrix()) {
        
        #set inversematrix to null
        inversematrix <- NULL
        
        #get the matrix passed to makeCatchMatrix
        get <- function() x
        
        #setsolve will be called during catchSolve
        #setsolve catches the inversematrix after the first call
        setsolve <- function( solve ) inversematrix <<- solve
        
        #get the solved matrix = inverse matrix
        getsolve <- function() inversematrix
        
        list(get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# cacheSolve: this function looks 'in' makeCacheMatrix for the result of solve() if
# if there is a solution present in inveresematrix then it returns it. If no solution 
# is present then it calculates solve() and stores the result in the cache created in 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        #looks into makeCatchMatrix and returns the value of the solved matrix, 
        # in this case, the inverse matrix
        inversematrix <- x$getsolve()
        
        # if the cached value is !null return it along with the comment "getting cached data"
        if(!is.null(inversematrix)) {
                message("getting cached data")
                return(inversematrix)
        }
        
        # if x$getsolve is was null calculate the inverse/solve
        data <- x$get()
        inversematrix <- solve(data) #this is the calcuation
        x$setsolve(inversematrix) # store the solution in the cache
        
        # return the inverse
        inversematrix
}