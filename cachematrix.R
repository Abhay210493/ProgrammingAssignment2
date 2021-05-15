## Comments as per my understanding

## Below function will create a special matrix which is required to be inversed and stored in cache memory.
## makeCacheMatrix function creates the matrix while the next function cacheSolve, creates the inverser of required
## matrix and stores in a cache variable. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                            ## Creating a inv variable and assigning NULL to it so as define it as an argument in future functions
  set <- function(y, ...)                ## A set function to set the values of variable x and inv in global environment
  {
    x <<- y                              ## Using <<- operator, here I have set value of x to the input argument of previous function in global enviriment
    inv <<- NULL                         ## Now inv variable is set to null in global environment so that when we create a new matrix, all previous values of inv is deleted
  }
  get <- function () x                   ## Now we will assign value of matrix x in global environment to a variable get so as to use this value in future
  setinv <- function (inv2) inv <<- inv2 ## now we create a function to set the value of inverse which we shall receive from CacheSolve or directly assinged. 
  getinv <- function() inv               ## Now we actually get the value of inverse and as inv is outside function parentesis, we will get value from global envirnoment
  list ( set = set, get = get, setinv = setinv, getinv = getinv) ## Now from this list we can access all the values using $ operator along with the martix variable we create. 
}


## This function actually inverses the required matrix and sets the data in cache memory.

cacheSolve <- function(x, ...) {          ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                       ## This will assign inv the value of getinv of x. If we have previously solved x then getinv() wont be NULL. But if we have not solved, it will be NULL
  if (!is.null(inv))                      ## if inv is not NULL that is if we have solved for inv before then this loop will run and no computation shall be done
    {
    message ("Getting Cache Data")
    return(inv)
  } ## Below will be solving for inverse if inv is not NULL
  data <- x$get()                         ## This will assign data from makeCacheMatricx function for further process. ( Can be done without but will create confusion)
  inv <- solve(data, ...)                 ## solve for inverse and store in inv which shall be send to setinv() function in makeCacheMatrix  
  x$setinv(inv)                           ## Set inv solved value to set it in global environment
  inv                                     ## Return the inverse value computated and not from the cache memory
}

