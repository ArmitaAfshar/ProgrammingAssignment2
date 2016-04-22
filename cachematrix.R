
#create a matrix object with 4 functions to set/retrieve the matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
	  list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}

#a function to calculate the inverse of a matrix
cacheSolve <- function(x, ...) {

        data <- x$get()
        m1 <- solve(data, ...)

        m2 <- x$getInv()
        #check if matrix inverse is changed
        if((!is.null(m2))&&(m1==m2)) 
	  { 	
            message("Getting the inverse from the cache:")
            return(m1)
        }

        x$setInv(m1)
        m1
}
