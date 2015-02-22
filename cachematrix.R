##functions intended to pull in a matrix, inversing said matrix 
##and then caching the inverse of the matrix

##The following function handles taking a matix in and caching


makeCacheMatrix <- function(x = matrix()) {    
        
        m<-NULL ##initializes m (in event cachesolve not called)
        y<-NULL ##initializes y variable
        
        setcache<-function(y){
                
                x<<-y ##caching to see if cachesolve returns a different value
                m<<-NULL ##reinitialize m
        }
        
        getcache<-function() x ##gets cached matrix
        setmatrix<-function(solve) m<<- solve ##inverts data in m
        getmatrix<-function() m ##m stores cached matrix
        
        list(setcache=setcache, getcache=getcache,
             setmatrix=setmatrix,
             getmatrix=getmatrix) ## creates list to store functions
}

##function that evaluates the cached matrix and either lets user know that matrix 
##was previously cached and prints or asigns matrix to variable


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix() ##sets m = to cached matrix
        NewMatrix <- x$getmatrix()
        
        ##print(m)
        
        
        ##if statement that attempts to evaluate if the m matrix is not null and 
        ##if m matrix is equal to its previous value then let user know that
        ##m has been cached and retrieving values and rint m...eles get inverse
        ##matrix values from makeCacheMAtrix
        if(!is.null(m)){ 
                matequal <- function(a, b)
                        is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
                if (matequal(NewMatrix, matrix)) {
                        if (matequal(NewMatrix, matrix)) {
                                message("getting cached data")
                                return(m)   
                        }
                        
                }
                message("getting cached data")        
        }
        
        matrix <- x$getcache() ##if m is null it sets temporary variable to the original matrix
        
        ##inverts matrix dataset and sets m = to that value
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m ##prints m
        ##NewMatrix <- m ##stores last value of m in newmatix
        
}
