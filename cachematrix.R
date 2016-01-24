# Caching the Inverse of a Matrix:

# In running time-consuming computations, it is a good practice to cache the results so 
# that you can look them up later instead of computing them again. Good example of these
# is matrix inversion. Dealing with big size matrix and running into thousands of 
# iterations, indeed, will cost you something, in terms of computation. That is why there 
# may be some benefit to cache the inverse of a matrix rather than compute it repeatedly. 
# The following two functions are used to create a special object that stores a matrix 
# and caches its inverse.


# The first function, makeCacheMatrix, creates a special "matrix" object that caches its
# inverse. It returns a list which will be used as the input in the second function, 
# cacheSolve(). This list contains four functions, where:
# 1) the first sets the matrix
# 2) the second gets the matrix
# 3) the third sets the inverse of the matrix
# 4) the fourth gets the inverse of the matrix

makeCacheMatrix<-function(x=matrix()) 
{
	inv_x<-NULL
      set<-function(y) 
	{
      	x<<-y
            inv_x<<-NULL
	}
      get<-function()
	{
		x
	}
      setInv_x<-function(inverse)
	{
		inv_x<<-inverse
	}
      getInv_x<-function()
	{
		inv_x
	}
      list(set=set, get=get, setInv_x=setInv_x, getInv_x=getInv_x)
}


# The second function, cacheSolve, computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. First of all, it checks if the inverse has already been computed. 
# If it has, it gets the result and will not do the computation. If it has not, it calculates 
# the inverse and sets the value in the cache thanks to setInv_x function.
# This function assumes that the matrix is always invertible.

cacheSolve<-function(x, ...) 
{
	inv_x<-x$getInv_x()
      if(!is.null(inv_x)) 
	{
      	message("getting cached data")
            return(inv_x)
	}
      data<-x$get()
      inv_x<-solve(data, ...)
      x$setInv_x(inv_x)
      inv_x
}


# Example on a sample:

# > sample<-makeCacheMatrix(matrix(c(1,0,1,2,-1,3,1,4,2),3,3))
# > sample$get()
#      [,1] [,2] [,3]
# [1,]    1    2    1
# [2,]    0   -1    4
# [3,]    1    3    2
# > sample$getInv_x()
# NULL
# > cacheSolve(sample)
#      [,1] [,2] [,3]
# [1,]  2.8  0.2 -1.8
# [2,] -0.8 -0.2  0.8
# [3,] -0.2  0.2  0.2
# > cacheSolve(sample)
# getting cached data
#      [,1] [,2] [,3]
# [1,]  2.8  0.2 -1.8
# [2,] -0.8 -0.2  0.8
# [3,] -0.2  0.2  0.2
# > sample$getInv_x()
#      [,1] [,2] [,3]
# [1,]  2.8  0.2 -1.8
# [2,] -0.8 -0.2  0.8
# [3,] -0.2  0.2  0.2
# > sample$set(matrix(c(1,1,2,2,1,2,1,-2,1),3,3))
# > sample$get()
#      [,1] [,2] [,3]
# [1,]    1    2    1
# [2,]    1    1   -2
# [3,]    2    2    1
# > sample$getInv_x()
# NULL
# > cacheSolve(sample)
#      [,1] [,2] [,3]
# [1,]   -1  0.0  1.0
# [2,]    1  0.2 -0.6
# [3,]    0 -0.4  0.2
# > cacheSolve(sample)
# getting cached data
#      [,1] [,2] [,3]
# [1,]   -1  0.0  1.0
# [2,]    1  0.2 -0.6
# [3,]    0 -0.4  0.2
# > sample$getInv_x()
#      [,1] [,2] [,3]
# [1,]   -1  0.0  1.0
# [2,]    1  0.2 -0.6
# [3,]    0 -0.4  0.2