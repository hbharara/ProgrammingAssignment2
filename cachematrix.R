## Assignment: 2 Caching the Inverse of the Matrix

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## The script has a pair of functions that cache the inverse of a matrix


## Function 1: makecacheMatrix
## Make Cache Matrix is a function that outputs a
## linear vector of list containting functions

## Variable Definition
## init : init is a matrix object and is a local variable limited to the scope of the function
## invMatrix : Global Variable responsible for storing the inverse of the input matrix
## set : the function is called if the users wants to directly initialize the init variable
## get : Function responsible for outputing the input matrix
## setInv : function responsbile for calculating and assigning the inverse of the matrix
## getInv : Function responsible for output of the calculated inverse of the matrix
## TempMatrix : A Temporary Matrix Object that gets assigned while calling the set() function



makecacheMatrix <- function(init=matrix())
{


	invMatrix <- matrix()
	set <- function(TempMatrix=matrix())
		{
			init <<- TempMatrix
			invMatrix <<- matrix()
		}

	get    <- function(){init}
	setInv <- function(setInv){ invMatrix <<- solve(init) }
	getInv <- function() { invMatrix }
	
	list(get = get , setInv = setInv , getInv = getInv)			

}


## Function 2: cacheSolve
## cachesolve is a function that calculates the Inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

## Variable Definition
## init : init is a matrix object and is a local variable limited to the scope of the function
## invMatrix : Global Variable responsible for storing the inverse of the input matrix
## initFunc : input variable of list class 
## dataMatrix : variable of matrix class responsible for storing output of 'get()' function

cacheSolve <- function(initFunc)
{
invMatrix <- initFunc$getInv()

	if (all(!is.na(invMatrix)))
	   {
		print("Printing Cached Inverse Matrix")
		print(invMatrix)
	   }
	else
	   {
		dataMatrix <- initFunc$get()
		invMatrix <- solve(dataMatrix)
		initFunc$setInv(invMatrix)
		print(invMatrix)
	   }	
}
