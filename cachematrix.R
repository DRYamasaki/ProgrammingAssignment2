	## Programming Assignment 2 

	## This assigment contains 2 functions, the first fuction (makeCacheMatrix.R) will cache results
      ## of the inverse of a matrix created by the second function (cacheSolve.R)
	
	## 	             Function 1 makeCacheMatrix.R
	## This funcion creates a special "matrix" object that can cache 
	## its inverse.  The function does not calculate the inverse, it simply stores values.
	## The argument for this funcion is a matrix.
      

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {				## set stores value
                x <<- y
                i <<- NULL				## restores to null beause the old inverse is no loner needed
        }
        get <- function() x				## get retrievs value
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,	
             getinverse = getinverse)	
}

	##   example command  > a<-makeCacheMatrix(matrix(c(1,1,1,3,4,3,3,3,4),3,3))
	##				Argument is a 3x3 matrix       1 3 3
	##									 1 4 3
	##									 1 3 4


	##		Function 2 cacheSolve.R
	## This function calculates the invers of a matrix using the solve function after first checking to see if the inverse already exists in memory.  
	## If the inverse does already exist, the function simply retrieves the results.  Its argument is the output from the function makeCacheMatrix.R

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
	## example command (using output from makeCacheMatrix above  > cacheSolve(a)  
	##
	## Results for this example will be a 3 x 3 matrix
	##					 7 -3 -3
	##					-1  1  0
	##					 4  0  1
