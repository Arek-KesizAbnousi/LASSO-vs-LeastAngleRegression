library(glmnet)
library(glmnet)
lasso_fit <- glmnet(X,y,alpha=1)
plot(lasso_fit)

#We are fitting LASSO using The "lars" function:
library(lars)
lasso_lars_fit <- lars(X,y,type = "lasso")
coeff_table_lasso_lars <- coef(lasso_lars_fit)
colnames(coeff_table_lasso_lars) <- c("beta1","beta2","beta3","beta4","beta5")
rownames(coeff_table_lasso_lars) <- c("step1","step2","step3","step4","step5","step6")
coeff_table_lasso_lars
