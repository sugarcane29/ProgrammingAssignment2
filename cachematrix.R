## The functions create a matrix that stores its inverse and then when the inverse is called, it checks if 
## it has the solved matrix stores. If found, it returns the value from cache, else will compute and return.

## The function creates a matrix and then calculates its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	
	}
	get<-function() {x}
	setinverse <- function(solved) {m<<-solved}
	getinverse <- function() {m}
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## The function takes the input as a matrix, checks if the inverse exists. If yes, it will return from cache,
## else will compute and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m<-x$getinverse()
		if(!is.null(m)){
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}
