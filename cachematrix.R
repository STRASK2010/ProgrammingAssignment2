project_path = file.path(Sys.getenv('HOME'), 'R Programming/ProgrammingAssignment2')
setwd(project_path)

##This function creates a special "matrix" object that can cache its inverse

## makeCacheMatrix creates the matrix and inverse, storing in cache

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ## Function 1
                x <<- y
                i <<- NULL
        }
        get <- function() x ## Function 2
        setInverse <- function(solve) i <<- solve  ## Function 3 Inversion
        getInverse <- function() i         ## Function 4
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) ## Creates a list of the 4 functions
}


## Solves the inverse of makeCacheMatrix if the answer is not in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()           #query the x vector's cache         
        if(!is.null(i)) {           #if there is a cache
                message("getting cached data") 
                return(i)                #just return the cache, no computation needed
        }
        data <- x$get()             #if there's no cache
        i <- solve(data, ...)        #we actually compute them here
        x$setInverse(i)                #save the result back to x's cache
        i                          #return the result
}
