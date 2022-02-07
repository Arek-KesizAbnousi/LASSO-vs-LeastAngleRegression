#---------------------------------------------------------------------------- 
#make matrix to store the parameters that will be returned from the function
#---------------------------------------------------------------------------- 

beta.hat <- rep(0,p+1)
gamma <- NULL #   gamma from the question
?? <- 1e-8
#------------
#Initial (Null) step
#-------------
r=y
#----------------------------------------------------------------------
#Calculating the Inner product and choosing 1st variable
#----------------------------------------------------------------------
inner_prod <- t(X)%*%r
j.hat <- which.max(abs(inner_prod))
#  ith step of the code
i=1
while ((i==1)||(gamma[i-1]<1))
{
  beta.hat <- rbind(beta.hat,rep(0,p+1))
  gamma <- c(gamma,1)
  njhat <- length(j.hat)
  j.hat <- c(j.hat,0)
  XT <- X[,j.hat]
  d <- rep(0,p)
  d[j.hat] <- solve(t(XT)%*%XT)%*%t(XT)%*%r
  X_d <- X%*%d
  #------------------------
  #Calculate Gamma for ith step of LARS algorithm
  #------------------------
  for (j in 1:p){
    alpha=1
    if (j%in%j.hat==FALSE){
      if (abs(sum(X[,j.hat[1]]*r)-sum(X[,j]*X_d))>??){
        alpha <- (sum(X[,j.hat[1]]*r)-sum(X[,j]*r))/
          (sum(X[,j.hat[1]]*r)-sum(X[,j]*X_d))
        if ((alpha<??)|(alpha>1-??)){
          alpha <- 1
          if (abs(sum(X[,j.hat[1]]*r)+sum(X[,j]*X_d))>??){
            alpha2 <- (sum(X[,j.hat[1]]*r)+sum(X[,j]*r))/
              (sum(X[,j.hat[1]]*r)+sum(X[,j]*X_d))
            if ((alpha2>??)&(alpha2<1-??))
              alpha <- alpha2
          }
        }
        if (alpha+??<gamma[i]){
          gamma[i] <- alpha
          j.hat[njhat+1] <- j
        }
      }
    }
  }
  beta.hat[i+1,2:(p+1)] <- beta.hat[i,2:(p+1)]+gamma[i]*d
  r <- r-gamma[i]*X_d
  i <- i+1
}
#------------------------
#We store the values of  coefficients (not normalized) in the defined matrix 
#------------------------
tilde.beta <- beta.hat[,-1]

# estimated parameter value (Stepwise):

coeff_table <- as.table(tilde.beta)
rownames(coeff_table) <- c("step1","step2","step3","step4","step5","step6")
colnames(coeff_table) <- c("beta1","beta2","beta3","beta4","beta5")
coeff_table
