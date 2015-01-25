# This R code has two functions listed below that works together to cache the inverse of a given matrix:

# 1. makeCacheMatrix:
# The first function takes in an invertible matrix as an argument and returns a list of four functions. These four functions (described along ...
# with their function definitions) are to set new data (a matrix), get the data, set the inverse to the matrix data, and to get that inverse
makeCacheMatrix <- function(x = matrix()) { 			# Takes an invertible matrix as an argument
		inv <- NULL										# Resets the 'inv' value, which is the cached inverse
        setdata <- function(y) {						# Access this function to change the original matrix, using the returned value of makeCacheMatrix
                x <<- y									# The environment of makeCacheMatrix has the matrix and inverse data that need to be changed
                inv <<- NULL							# <<- is used in both lines as we are an extra function 'deep' and so the environment has changed
        }
        getdata <- function() x							# Returns the matrix stored
        setinv <- function(inverse) inv <<- inverse		# Sets the inverse value in cache as the passed argument 'inverse'
        getinv <- function() inv						# Returns the inverse stored
        list(setdata = setdata, getdata = getdata,		# makeCacheMatrix returns the four functions that are defined within itself
             setinv = setinv,
             getinv = getinv)
}

# 2. cacheSolve:
# The second funtion takes an argument that has to be the returned value (say, f1ret) of ...
# the first function. f1ret can access the functions defined in the first function and ...
# so a check is performed on whether the inverse already exists in the first ... 
# function's environment or not. If yes, the already cached inverse is returned. If not ...
# the inverse is computed for this new matrix which obviously changed after the last function call
cacheSolve <- function(f1ret, ...) {					# Takes an argument that has to be the returned value of the first function

        inverse <- f1ret$getinv()						# Gets the inverse of the matrix, potentially cached already
        if(!is.null(inverse)) {							# If already cached, returns the inverse with a message. Else, proceed
                message("getting cached inverse")
                return(inverse)
        }
        mat <- f1ret$getdata()							# Inverse is not cached. First gets the matrix itself which should have ...
														# obviously been changed through the 'setdata' function of makeCacheMatrix
        inverse <- solve(mat, ...)						# Now computes the inverse of the matrix obtained using the R function 'solve'
        f1ret$setinv(inverse)							# Now sets the inverse in the environment of makeCacheMatrix using the 'setinv' function
        inverse											# cacheSolve returns this inverse thus computed
}
