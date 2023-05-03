
library(mvtnorm)
set.seed(1)

load("DataFrame_1.Rdata")

#Maximum Rating 
R <- df$MaximumRating

#Total Number of Problems Solved
N <- df$Problems.Solved.All.Time



# Parametric Bootstrapping
Parametric_boot <- function(p,q)
{
  num <- numeric(length = 1e4)
  n <- length(p)
  m1 <- mean(p)
  m2<- mean(q)
  var1 <- var(p)
  var2 <- var(q)
  covar <- cov(p, q)
  
  for(i in 1:1e4)
  {
    temp <- rmvnorm(n,mean = c(m1,m2),sigma <- matrix(c(var1,covar,covar,var2),nrow = 2,ncol = 2))
    num[i] <- cor(temp[,1], temp[,2])
    
  }
  
  return(num)
}


alpha <- 0.05
bounds <- c(alpha/2,1-alpha/2)

nums_para = Parametric_boot(R , N)
quantile(nums_para,bounds)


# Non-Parametric Bootstrapping
NonParametric_boot <- function(p,q)
{
  num <- numeric(length = 1e4)
  n <- length(p)
  for(i in 1:1e4)
  { 
    index <- sample(1:n,replace = TRUE)
    p_i <- p[index]
    q_i <- q[index]
    num[i]<- cor(p_i,q_i)
  }
  return(num)
}


alpha <- 0.05
bounds <- c(alpha/2,1-alpha/2)

num_nonpara <- NonParametric_boot(R,N)
quantile(num_nonpara,bounds)



# p-value calculation
pval <- function(p,q)
{
  n <- length(p)
  count = 0
  m1 <- mean(p)
  m2<- mean(q)
  var1 <- var(p)
  var2 <- var(q)
  corel = cor(p,q)
  
  for(i in 1:1e4)
  {
    temp <- rmvnorm(n,mean = c(m1,m2),sigma <- matrix(c(var1,0,0,var2),nrow = 2,ncol = 2))
    c <- cor(temp[,1],temp[,2])
    if(c > corel ){
      count  = count+1
    }
  }
  
  return(count/1e4)
}

pvalue <- pval(R , N)

pvalue
