#---------------------------------------
#Programming LARS by hand with p = 5
#---------------------------------------
set.seed(7934)
p=5; n=100
X.o <- matrix(rnorm(n*p), nrow=n, ncol=p)
beta <- rnorm(5)
y.o <- X.o%*%beta + rnorm(n)
# ---------------------------------------------
# standardize X.o and y.o
# ---------------------------------------------
X.sds <- apply(X.o,2,sd)
X <- (X.o- rep(1, n)%*%t(colMeans(X.o)))/(rep(1, n)%*%t(apply(X.o,2,sd)))
y <- y.o - mean(y.o)#
#----------------------------------------------
#tilde.beta: matrix along the solution
#----------------------------------------------
tilde.beta <- matrix(0, p, 6) # columns correspond to steps along the path

