#Plot of the solution path along the steps of the LARS algorithm
color <- rainbow(p)
plot(tilde.beta[,1],type = "l",ylim=c(-1,2),col=color[1],main = "Least Angle Regression",
     xlab = "Steps", ylab = "Coefficients")
for(i in 2:p)
{
  lines(tilde.beta[,i],type = "l",col=color[i])
}
