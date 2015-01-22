##### NOTE TO REVIEWERS #####
## Please excuse the mess. I've tried to be diligent in notating my code cleanly and explicitly as I am a complete newbie to R and coding in general.
## But I realize it can be a bit much to look at. 



## Summary: Creates a "special" matrix (a matrix with subfunctions) that can be called upon by name.
##          Current subfunctions are setting the input matrix, (re)setting the matrix while clearing the computed inverse matrix, 
##                and retreiving the matrix loaded when the function was called.
##	    Calling the 'makeCacheMatrix' directly should provide the same result as subfunction 'set'



## Example Commands for use:
##> tm <- matrix(c(4,3, 1,1), nrow = 2, ncol = 2)
##> make <- makeCacheMatrix(tm)
##> make$set(y)    ## where y is the new matrix to be used
##> make$get()
##> make$envinfo

makeCacheMatrix <- function(input = matrix()) {
	
	im <<- NULL 		## Creates a mutable symbol for the inverse matrix
	m <<- input		## Sets the input to a mutable symbol for the matrix which is then filled by input.
	
	
	mcheck <<- input	## Used as a novice attempt at making a failsafe against the user accidentally changing m after running the makeCacheMatrix and computing the wrong results.
	## Should be removed when using very large matrices as we are essentially storing two copies. 
	## A stored hash check would likely be a better option.
	
	
	env <- environment()	## Stores the environment information for use later.
	envname <- parent.env(env)	## Uses the environment information to determine the parent environment
	envlist <- ls(envname)	## Uses the environment information to list all symbols used in the parent environment
	
	set <- function(y) {	## Changes the input matrix with the provided argument matrix (m <<- y), and resets the the computed inverse matrix value to NULL (im <<- NULL)
		m <<- y
		im <<- NULL
	}
	
	get <- function() {	## prints the matrix loaded when using the 'makeCacheMatrix' to create the assigned symbol.
		return(m)
	}
	
	envinfo <- function() {	## prints the current function environment as well as the items in the environment.
		message("Working environment is:")
		print(envname)
		message("Environment Contents:")
		print(envlist)
		
	}
	
	list(set = set,         ## creates a list of named variables that reference the nested function of makeCacheMatrix. 
	     get = get,         ## Formatted for easier reading.
	     envinfo = envinfo) 
}

## **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~** ##

## Summary: Checks for the 'im' (inverse matrix) value to see if it has already been calculated. 
##		If it has been calculated, and the matrices match between 'm' and 'mcheck', then it will use the cache. If not, then the inverse matrix will be computed using solve()


## Example Commands for use:
##> cacheSolve(make)  ##presuming use of previously generated 'make' using makeCachMatrix

cacheSolve <- function(x, ...) {
	if(!is.null(im)) {			## Checks if the symbol 'im' is still set to NULL (i.e. not yet computed)
		if(exists("mcheck") == TRUE) {		## Checks to see if 'mcheck' exists (incase it was removed from the makeCacheMatrix function)
			if(identical(m, mcheck)) {	## Checks to make sure the 'm' and 'mcheck' failsafe are equal.
				## NOTE!	        	   Use of the identical function presumes that mcheck hasn't been changed. It is also not likely a good idea if the matrices are very large.
				message("Cache Value Found. Environmental matrix and argument matrix are identical. Using cached inverse matrix:")
				return(im)			## Shows the user the matrix being used.          	
				## NOTE!                             The return(im) should be disabled if used on large datasets. This was added as a debug/learning tool.
				
			} else {
				message("Source matrix ('m') does not match matrix in called argument. Recomputing inverse matrix")       
				im <<- solve(mcheck)		## Stores the computed inverse matrix into symbol 'im' 
				message("Value of the inverse matrix:")
				return(im)
				## NOTE!                             The return(im) should be disabled if used on large datasets. This was added as a debug/learning tool.	
			}
		}
		
	} else {			## If the 'im' value is NULL, then R should use the solve function to compute the inverse matrix.
		message("Cache not found. Computing inverse matrix.")       
		im <<- solve(x$get())		## Stores the computed inverse matrix into symbol 'im' 
		message("Value of the inverse matrix:")
		return(im)
		## NOTE!                     The return(im) should be disabled if used on large datasets. This was added as a debug/learning tool.
	}
}