makeCacheMatrix <- function(x = matrix()) {
        # Set an variable m for input matrix
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        #It consists of 3 parts: get x, set matrix, and get matrix 
        get<-function() x
        MatSet<-function(solve) m<<- solve
        MatGet<-function() m
        
        list(set=set, get=get,
             MatSet=MatSet,
             MatGet=MatGet)
}

cacheSolve <- function(x=matrix(), ...) {
        m<-x$MatGet()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        #These 4 lines are about getting, solving, setting, and printing matrix
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$MatSet(m)
        m
}
