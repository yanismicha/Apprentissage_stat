## ---- echo = FALSE-------------------------------------------------------
# define global chunks options
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

## ------------------------------------------------------------------------
load("regression-dataset.Rdata")
fit  = lm(y ~ x)

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
#preds = predict(fit, newdata = x.test) # does not work
preds = predict(fit, newdata = data.frame("x"=x.test))

## ------------------------------------------------------------------------
fit3 = lm(y ~ poly(x,3,raw=TRUE))
fit3$coefficients
# more details with summary(fit3)

## ------------------------------------------------------------------------
preds3 = predict(fit3, newdata = data.frame("x"=x.test))

## ------------------------------------------------------------------------
x.grid = seq(min(x),max(x),by=0.01)
fx.grid = predict(fit3, newdata = data.frame("x"=x.grid))
plot(x.grid, fx.grid, type = "l")
points(x, y, pch = 19)

## ------------------------------------------------------------------------
#############################
#### STARTING EXERCICE 1 ####
#############################
# load dataset
load("regression-dataset.Rdata")
# plot
plot(x, y, pch = 19, main = "dataset")
fit  = lm(y ~ x)
abline(fit, col = 2)

## ------------------------------------------------------------------------
plot(x, fit$residuals, xlab = "x", ylab = "residuals", pch = 19)
abline(h=0, lty = 2, col = 2)

## ------------------------------------------------------------------------
# specify polynomial degrees to consider
deg.list = c(1:5,8,10,15,20)
# compute MSE
MSE = c()
for(d in deg.list){
    fit = lm(y ~  poly(x,d,raw=TRUE))
    mse = mean( (y - fit$fitted.values)^2 )
    cat("*** degree", d, ": MSE on training data =", mse, "***\n")
    MSE = c(MSE, mse)    
}
# plot
plot(deg.list, MSE, type ="b", xlab = "polynomial degree", ylab = "MSE", main = "MSE vs degree of polynomial", pch = 19)

## ------------------------------------------------------------------------
MSE.test = c()
for(d in deg.list){
  fit = lm(y ~ poly(x,d,raw=TRUE))
  preds = predict(fit, newdata = data.frame("x"=x.test))
  mse = mean( (y.test - preds)^2 )
  cat("*** degree", d, ": MSE on test data =", mse, "***\n")
  MSE.test = c(MSE.test, mse)   
}
# plot
plot(deg.list, ylim = range(c(MSE,MSE.test)), MSE, type ="b", xlab = "polynomial degree", ylab = "MSE", main = "MSE vs degree of polynomial", pch = 19)
lines(deg.list, MSE.test, type = "b", col = 2, pch = 19)

## ---- fig.height = 10, fig.width = 10------------------------------------
# define  colors
library(RColorBrewer)
cols = brewer.pal(length(deg.list), "Set1")
# define grid of x values
x.grid = seq(0,10,by=0.01)
# define mfrow
par(mfrow = c(2,2))
# plot models  - all on the same graph
plot(x,y, pch = 19, main = "model fits - all degrees")
for(i in seq(length(deg.list))){
  d = deg.list[i]
  fit = lm(y ~ poly(x,d,raw=TRUE))
  preds = predict(fit, newdata = data.frame("x"=x.grid))
  lines(x.grid, preds, col = cols[i], lwd = 2)
}
legend("bottomright", paste("d =",deg.list), col = cols, lwd = 2, cex = 0.8)
# plot intermediate models
deg.list.small = c(3,10,20)
for(d in deg.list.small){
  fit = lm(y ~ poly(x,d,raw=TRUE))
  preds = predict(fit, newdata = data.frame("x"=x.grid))
  plot(x, y, pch = 19, main = paste("model fit - degree", d)) 
  lines(x.grid, preds, col = cols[which(deg.list==d)], lwd = 2)
}
par(mfrow = c(1,1))

## ---- fig.width=9, fig.height=9------------------------------------------
display.brewer.all()

## ------------------------------------------------------------------------
#############################
#### STARTING EXERCICE 2 ####
#############################
y = as.character(read.table("datasets/nci.label")$V1)
X = read.table("datasets/nci.data")
X = t(X)
tt = sort(table(y), decreasing = TRUE)
par(mar = c(7,4,4,2))
barplot(tt, main  = "number of observations per class", las = 2)
par(mar = c(5,4,4,2))

## ------------------------------------------------------------------------
ind1 = grep("repro", y)
ind2  = which(y == "UNKNOWN")
ind.rm = c(ind1, ind2)
y = y[-ind.rm]
y = factor(y)
X = X[-ind.rm,]

## ------------------------------------------------------------------------
pca = prcomp(X)
cat("*** number of PCs =", ncol(pca$x), "***\n")

## ---- fig.height = 6, fig.width = 6--------------------------------------
# define  colors
cols = brewer.pal(nlevels(y), "Set1")
# plot
plot(pca$x[,1], pca$x[,2], pch = 19, col = cols[y], xlab = "PC1", ylab = "PC2", main = "Firt two PCs")
legend("topleft", levels(y), col = cols, pch = 19)

## ---- fig.height=4, fig.width=8------------------------------------------
par(mfrow = c(1,2))
# plot
boxplot(pca$x[,1] ~ y, main = "PC1 vs class", las = 2)
# improved plot
tt = split(pca$x[,1], y)
ind.sort = order( sapply(tt, median) )
tt = tt[ind.sort]
boxplot(tt, main = "PC1 vs class - improved", las = 2)
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
boxplot(pca$x[,2] ~ y, main = "PC2 vs class", las = 2)

## ------------------------------------------------------------------------
# compute and cast to percent
prop.var = pca$sdev^2
prop.var = prop.var  / sum(prop.var)
prop.var = round(100*prop.var, digits = 2)
# plot "scree"
plot(prop.var, type = "b", pch = 19, xlab = "PC index", ylab = "proportion of explained variance", main = "proportion of variance explained by PC")
grid()

## ------------------------------------------------------------------------
# plot cumulative
plot(cumsum(prop.var), type = "b", pch = 19, xlab = "number of PCs", ylab = "cumulative proportion of explained variance", main = "cumulative proportion of explained variance")
grid()
abline(h = 90, lty = 2)
abline(v = 30, lty = 2)

## ---- fig.width = 6, fig.height = 6--------------------------------------
#############################
#### STARTING EXERCICE 3 ####
#############################
rm(list = ls())
load("datasets/digits-3.Rdata")
# define colors
cols = gray(seq(1,0,length.out=256))
# show a few images
n = 5
par(mfrow = c(n,n))
par(mar = c(1,1,1,1))
set.seed(27)
ind.sple = sample(dim(I)[3], n*n)
for(i in 1:length(ind.sple)){
  image(I[,,ind.sple[i]], col = cols, axes = F)
  box()
}
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
X = apply(I, 3, function(x){as.vector(x)})
X = t(X)

## ------------------------------------------------------------------------
pca = prcomp(X)
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2", main = "PCA plot", pch = 19, col = "grey")

## ---- fig.height = 4, fig.width = 8--------------------------------------
I1  = matrix(pca$rotation[,1], nrow = 16)
I2  = matrix(pca$rotation[,2], nrow = 16)
par(mfrow = c(1,2))
image(I1, col = cols, main = "first principal component", axes = F)
box()
image(I2, col = cols, main  = "second principal component", axes = F)
box()
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2", main = "PCA plot", pch = 19, col = "grey")
# show points
points(pca$x[ind.grid,1], pca$x[ind.grid,2], col = 2, pch = 19)
abline(h = c(-5,-2.5,0,2.5,5), lty = 2)
abline(v = c(-5,-2.5,0,2.5,5), lty = 2)

## ---- fig.height = 6, fig.width = 6--------------------------------------
# show corresponding images
par(mfrow = c(5,5))
par(mar = c(1,1,1,1))
for(i in 1:5){
  for(j in 1:5){
    image(I[,,ind.grid[i,j]], col = cols, axes = F)
    box()
  }
}

## ---- fig.width=8, fig.height=4------------------------------------------
#############################
#### STARTING EXERCICE 4 ####
#############################
library(MASS)
# define mean vector
mu = c(2,3)
# define number of samples to draw
n = 200
# draw - spherical
S1 = matrix(c(1,0,0,1), nrow = 2, byrow = T)
X1 = mvrnorm(n, mu, S1)
# draw = diagonal
S2 = matrix(c(4,0,0,1), nrow = 2, byrow = T)
X2 = mvrnorm(n, mu, S2)
# draw = ellipsoidal
S3 = matrix(c(2,1,1,1), nrow = 2, byrow = T)
X3 = mvrnorm(n, mu, S3)
# plot
par(mfrow = c(1,3))
xlim = range(c(X1,X2,X3))
plot(X1[,1],X1[,2], xlim = xlim, ylim = xlim, pch = 19, main = "spherical")
grid()
plot(X2[,1],X2[,2], xlim = xlim, ylim = xlim, pch = 19, main = "diagonal")
grid()
plot(X3[,1],X3[,2], xlim = xlim, ylim = xlim, pch = 19, main = "ellipsoidal")
grid()
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
library(mclust)
fit1 = mvn("XII", X1)
cat("*** spherical distribution *** \n")
cat("t- estimated mean =", fit1$parameters$mean, "(vs mu =", mu, ")\n")
cat("t- estimated Sigma =", fit1$parameters$variance$Sigma, "(vs Sigma =", S1, ")\n")

fit2 = mvn("XXI", X2)
cat("*** diagonal distribution *** \n")
cat("t- estimated mean =", fit2$parameters$mean, "(vs mu =", mu, ")\n")
cat("t- estimated Sigma =", fit2$parameters$variance$Sigma, "(vs Sigma =", S2, ")\n")

fit3 = mvn("XXX", X3)
cat("*** ellipsoidal distribution *** \n")
cat("t- estimated mean =", fit3$parameters$mean, "(vs mu =", mu, ")\n")
cat("t- estimated Sigma =", fit3$parameters$variance$Sigma, "(vs Sigma =", S3, ")\n")

## ------------------------------------------------------------------------
# number of dimensions to conisder
p  = seq(2, 10)
# number of parameters of the 3 models
n1 = p + 1
n2 = p + p
n3 = p + p*(p+1)/2
# plot
plot(p, n1, ylim = c(0, max(c(n1,n2,n3))),  type = "b",  pch = 19, xlab = "dimension", ylab = "number of  parameters", main = "number of parameters vs dimension")
lines(p, n2, type = "b", pch = 19, col = 2)
lines(p, n3, type = "b", pch = 19, col = 3)
legend("topleft", c("spherical","diagonal","ellipsoidal"), col = c(1,2,3), pch = 19)

## ------------------------------------------------------------------------
mv = function(X, mu, S){
 p = length(mu)
 return( 1/( sqrt((2*pi)^p*det(S)) ) *  exp( -0.5 * t(X-mu) %*% solve(S) %*% (X-mu)) )
}

## ------------------------------------------------------------------------
# define grid
x = seq(-2,6, by = 0.02)
y = x
# init density
D = matrix(0, nrow = length(x), ncol = length(y))
D.hat = D
# extract estimated parameters
mu.hat= as.vector(fit3$parameters$mean)
S3.hat = fit3$parameters$variance$Sigma
# compute densities
for(i in 1:nrow(D)){
  for(j in 1:ncol(D)){
    X = matrix( c(x[i],y[j]), nrow = 2, ncol = 1)
    D[i,j] = mv(X, mu, S3)
    D.hat[i,j]  = mv(X, mu.hat, S3.hat) 
  }
}

## ---- fig.width=6, fig.height=6------------------------------------------
# plot
plot(X3[,1], X3[,2], pch = 19, col = "grey")
contour(x,y,D, add = TRUE)
contour(x,y,D.hat, add = TRUE, col = "red")
legend("bottomright", c("densité théorique","densité estimée"), col = c("black","red"), lwd = 1)

