##The two functions below define an object - a matrix - and it's inverse. If 
##the inverse of input x has been calculated before, the cached inverse is returned, 
##otherwise the inverse matrix is calculated and stored in cache. The first function 
##creates the matrix and returns a list of functions (set, get, getcm and setcm) to
##the function envirnoment. This list includes the functions and the data-items that
##these functions were called upon, and hence othe the functions and the data
##are available in the parent environment of the function. That is why we can use 
##the data and fucntions in the subsequent function cacheSolve.  

## Within the makeCacheMatrix function, we initialize the cm matrix 
##and give it placeholder value NULL (i.e. our future inverse matrix). 
##Then define the set function to assign to matrix x the input y
##and reset the cm matrix to NULL, thus whenever a new input is given 
##to the matrix x, any stored cache for cm will be cleared.
##After that, we define the get function, that returns the matrix x, 
##and since x is not defined within the get function, 
##it gets it's value from it's parent environment, 
##i.e. from the makeCacheMatrix function. 
##The set function assigns the inverse matrix cachm 
##- that we will define below with the solve function- to cm. 
##Lastly, the functions are all defined in a list so that each item 
##can be used in subsequent code.

makeCacheMatrix <- function(x = matrix()) {
    cm<-matrix() ##probably not necessary
    cm <- NULL
    set <- function (y = matrix()){
          x<<-y
          cm<<-NULL
          }  
    get <- function () x 
    setcm <- function (cachm) cm <<-cachm 
    getcm <- function () cm 
    list(set=set, get=get,
         setcm=setcm,
         getcm=getcm)
    }
##Subsequently, we're going to 'recall' or create the inverse matrix for x
##the cacheSolve function first assigns the result of the getcm function 
##on x to cm. If there is indeed a result from getcm that is not NULL, than the
## cached (or previously 'setcm') result is provided. Otherwise, the cachm matrix 
## is created by the solve function on data matrix x that is called by the get function 
##below and subsequently this cachm matrix is input for the setcm function to 
##assign cachm to cm, and lastly return cm. The different functions get, set and 
##getcm and setcm can be called below since they are defined in parent environment 
##list above.
cacheSolve <- function(x, ...) {
    cm <- x$getcm()
    if(!is.null(cm)) {
      message("getting cached data")
      return(cm)
      } else {
      data <- x$get()
      cachm <- solve(data, ...)
      x$setcm (cachm)
      cm
      }
    }
## Return a matrix that is the inverse of 'x'



