#   The exact probability distribution of the rank product statistics for 
#   replicated experiments, by Eisinga, Breitling & Heskes, FEBS Letters, 
#   January 2013


#PILTZCOUNT Number of ways to construct a rank product
#   piltzcount(r,k,n) returns the number of ways rank product r can be
#   constructed from k experiments if n is the number of genes (and thus
#   the maximum rank in a single experiment)
#

# Save the unique rows of input matrix A in a new matrix C and create 
# an index vector ia, containing the sequence numbers of the rows in A
# that were saved in C. 

uniquerows <- function(A){
  C <- uniquecombs(A)
  ia <- vector("integer", length=nrow(C))
  for (i in 1:nrow(C)){
    for (j in 1:nrow(A)){
      score <- ifelse (C[i,]==A[j,], yes=1, no=0) 
      if(sum(score) == ncol(A)){ia[i] <- j; break}
    }
  }
  return(list(C,ia))
} # end function uniquerows 

#------------------------------------------------------------------------ 

# Extract prime factors and powers of rank product r. 

myfactor <- function(n){
  if (n==1){return(c(1,0))} 
  # Use factorize() from package gmp.  
  pf <- as.integer(factorize(n))
  f  <- unique(pf) 
  # Compute multiplicities.
  nprimes <- length(f)
  m <- matrix(nrow=nprimes, ncol=1, 0)
  for (i in 1:nprimes){ m[i] <- sum(pf == f[i]) }
  return(cbind(f,m))
} # end myfactor.

#------------------------------------------------------------------------

# Obtain all divisors of f1^m1 * f2^m2 * f3^m3 ...
# where f1, f2, f3 ... are primes and m1, m2, m3 prime powers. 

mydivisor <- function(f_and_m){
  f <- f_and_m[,1]; m <- f_and_m[,2]
  nprimes <- length(f)
  d <- matrix(f[1]^(0:m[1]), ncol=1)
  if (nprimes > 1){
    for (i in 2:nprimes){
      d <- d %*% f[i]^(0:m[i])
      d <- matrix(d, ncol=1)
    }
  }
  d <- sort(d)
  return(d)
} # end mydivisor

#-------------------------------------------------------------------------

# Calculate binomial coefficient. 
bincoeff <- function(n, k){
  y <- exp(lgamma(n+1)-lgamma(k+1)-lgamma(n-k+1))
  y <- round(y)
  return(y)
} # end bincoeff

#-------------------------------------------------------------------------

# Calculate multinomial coefficient. 
multcoeff <- function(n, x){
  y <- exp(lgamma(n+1)-sum(lgamma(x+1)))
  y <- round(y)
  return(y)
} # end multcoeff

#-------------------------------------------------------------------------

# Compute the number of k-tuples of rankproduct value r, given n genes. 

piltzcount <- function(r,k,n){
  
  #cat ("r=", r, " k=", k, " n=", n, "\n")
  
  # Check for trivial cases.
  if (k < 1)   {h <- 0; return(h)} else
    if (r == 1)  {h <-1;  return(h)} else
      if (r > n^k) {h <- 0; return(h)} else 
        if (k==1)    {h <- 1; return(h)}
  
  # Compute prime factorization.
  fm <- myfactor(r)
  f <- fm[,1]; m <- fm[,2]
  nprimes <- length(f)
  
  # Calculate first term: number of ordered k-tuples with product r.    
  h <- 1
  for (t in 1:nprimes){ h <- h * bincoeff(m[t]+k-1, k-1) }
  
  #cat("First term of h: ", h, "\n")  
  
  if (r > n){
    
    # Get all divisors of r.
    d <- mydivisor(fm)
    # Select divisors larger than n.
    bigones <- d[d>n]  # bigones is a row vector.
    nbig <- length(bigones)
    #print("bigones"); print(bigones)
    
    # Construct possible combinations of divisors.
    divset <- list()
    bbb    <- list()
    divset[[1]] <- 1
    bbb[[1]] <- matrix(0,nrow=1,ncol=nbig)
    
    # Compute maximum number of divisors that can be divided out: smax. 
    smax <- ceiling(log(r)/log(n)) - 1
    
    for (s in 1:smax){
      
      #print("s"); print(s)
      newdivset = matrix(divset[[s]], ncol=1) %*% bigones
      #print("newdivset"); print(newdivset)
      
      ij <- as.matrix(which(r - newdivset*floor(r/newdivset)<0.5,arr.ind=TRUE))
      #print(ij)
      i <- ij[,1]; j <- ij[,2]
      nnew <- length(i)
      #cat("nnew=", nnew,"\n")
      
      if (nnew > 0) {
        
        divset[[s+1]] <- matrix(0,ncol=nnew)
        bbb[[s+1]]    <- matrix(0,nrow=nnew,ncol=nbig)
        
        for (t in 1:nnew) {
          #print("***"); print(divset[[s]][i[t]])
          divset[[s+1]][t]   <- divset[[s]][i[t]] * bigones[j[t]]
          bbb[[s+1]][t,]     <- bbb[[s]][i[t],]
          bbb[[s+1]][t,j[t]] <- bbb[[s+1]][t,j[t]] + 1 
        } 
        #print("divset[[s+1]]"); print(divset[[s+1]]) 
        #print("bbb"); print(bbb)
        
        temp <- uniquerows(bbb[[s+1]])
        bbb[[s+1]] <- temp[[1]];  iii <- temp[[2]]
        #print("iii"); print(iii)
        divset[[s+1]] <- divset[[s+1]][iii]
        #print("divset"); print(divset)     
        
        for (t in 1:length(iii)){
          hextra <- piltzcount(r / divset[[s+1]][t], k-s, r)
          #cat("hextra=", hextra, "s= ", s, "divset[[s+1]][t]= ", divset[[s+1]][t], "\n")
          add_or_subt <- bincoeff(k,s) * multcoeff(s,bbb[[s+1]][t,]) * hextra
          #cat("add_or_subt= ", add_or_subt, "\n")
          h = h + (-1)^s * add_or_subt 
        }        
        
      } else {break}
      
    } # end s in 1:smax
    
  } #end if (r > n)
  
  return(h)
  
} # end piltzcount

#-------------------------------------------------------------------------

#--- Example 1.
# Calculate for n=500 the number of (k=)5-tuples with rankproduct rp=9720.
#piltzcount(900,14,1309)

# Gamma distribution approximation of p value for rank product rp=9720.
righttailgamma <- function(r,k,n) {
  1 - pgamma(-log(r/(n+1)^k),k,scale=1)
}


  #--- Example 2.
  # Calculate for n=500 the number of (k=)5-tuples with rankproduct rp<=9720.
  # Display the exact p-value.
#  total <- 0
 # for (n in 1:rp){
  #  total <- total + piltzcount(n,k,N)
  #}
  #return(total/N^k)
#}

