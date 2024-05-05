## ------------------------------------------------------------------------------------------------------------------------------------
#############################
#### STARTING EXERCICE 1 ####
#############################
load("datasets/tp-4_exo-1.Rdata")


## ------------------------------------------------------------------------------------------------------------------------------------
# question 2 - show confusion matrix and compute sensi/spec
  # 1st classifier
cat("1st classifier : confusion matrix\n")
table(data$label, data$score1 > 0)
sensi1 = mean(data$score1[data$label == 1] >= 0)
speci1 = mean(data$score1[data$label == 0] < 0)
cat("--> sensi =", sensi1, "; speci =", speci1, "\n")
  # 2nd classifier
cat("2nd classifier : confusion matrix\n")
table(data$label, data$score2 > 0)
sensi2 = mean(data$score2[data$label == 1] >= 0)
speci2 = mean(data$score2[data$label == 0] < 0)
cat("--> sensi =", sensi2, "; speci =", speci2, "\n")
# 3rd classifier
cat("3rd classifier : confusion matrix\n")
table(data$label, data$score3 > 0)
sensi3 = mean(data$score3[data$label == 1] >= 0)
speci3 = mean(data$score3[data$label == 0] < 0)
cat("--> sensi =", sensi3, "; speci =", speci3, "\n")


## ------------------------------------------------------------------------------------------------------------------------------------
# question 3 : compute ROC curves
library(ROCR)
pred1 = prediction(data$score1, data$label)
pred2 = prediction(data$score2, data$label)
pred3 = prediction(data$score3, data$label)
perf1 = performance(pred1, measure = "tpr", x.measure = "fpr")
perf2 = performance(pred2, measure = "tpr", x.measure = "fpr")
perf3 = performance(pred3, measure = "tpr", x.measure = "fpr")


## ---- fig.height=6, fig.width=6------------------------------------------------------------------------------------------------------
# question 3 : plot ROC curves
plot(perf1, main = "ROC curves")
plot(perf2, add = TRUE, col = 2)
plot(perf3, add = TRUE, col = 3)
abline(0,1, lty = 2)
legend("bottomright", paste("classifieur",c(1:3)), col = c(1:3), lty = 1)
# question 4 : show "default" sensi/speci values
points(1-speci1, sensi1, col = 1, pch = 19)
points(1-speci2, sensi2, col = 2, pch = 19)
points(1-speci3, sensi3, col = 3, pch = 19)
# question 5 : sensi/speci levels of 75%
abline(h = 0.75, lty = 2)
abline(v = 1-0.75, lty = 2)


## ------------------------------------------------------------------------------------------------------------------------------------
# 1st classifier
perf1.auc = performance(pred1, measure = "auc")
auc1 = perf1.auc@y.values[[1]]
cat("1st classifier, AUC =", auc1)
# 2nd classifier
perf2.auc = performance(pred2, measure = "auc")
auc2 = perf2.auc@y.values[[1]]
cat("2nd classifier, AUC =", auc2)
# 3rd classifier
perf3.auc = performance(pred3, measure = "auc")
auc3 = perf3.auc@y.values[[1]]
cat("3rd classifier, AUC =", auc3)


## ------------------------------------------------------------------------------------------------------------------------------------
#############################
#### STARTING EXERCICE 2 ####
#############################
load("datasets/tp-4_exo-2.Rdata")


## ------------------------------------------------------------------------------------------------------------------------------------
# question 2 : découper le jeu d'apprentissage en jeu de validation et jeu d'apprentissage
set.seed(27)
ind.val = sample(length(y), 2000)
X.val = X[ind.val,]
y.val = y[ind.val]
X.train = X[-ind.val,]
y.train = y[-ind.val]


## ------------------------------------------------------------------------------------------------------------------------------------
# question 3 : évaluer impact de k sur les performance de classification.
library(class)
# on considère des valeurs de k=1:10
k.list = seq(10)
# on crée un vecteur vide pour stocker les performances de classification
acc.val = rep(0, length(k.list))
# on évalue les performances de classification pour les différentes valeurs de k 
for(i in 1:length(k.list)){
  k = k.list[i]
  cat("*** processing k=", k, "***\n")
  preds.val = knn(X.train, X.val, y.train, k = k)
  acc.val[i] = mean(preds.val == y.val)
}


## ------------------------------------------------------------------------------------------------------------------------------------
# on trace les résultats
plot(k.list, acc.val, type = "b", pch = 19, xlab = "k", ylab = "accuracy", main = "validation performance vs value of k")
k.best = k.list[which.max(acc.val)]
cat("*** best value of k found by internal validation =", k.best, "***\n")


## ------------------------------------------------------------------------------------------------------------------------------------
# question 4 
k.best = k.list[which.max(acc.val)]
preds.test = knn(X, X.test, y, k = k.best)
acc.test = mean(preds.test == y.test)
cat("*** accuracy measured on test data =", acc.test, "***\n")


## ------------------------------------------------------------------------------------------------------------------------------------
# question 5 
# build a variable indicating wether the prediction is correct or not
pred.correct = y.test == preds.test
# split according to reference label
tt = split(pred.correct, y.test)
# accuracy by class = mean value of pred.correct, when split by class
acc.digit = sapply(tt,mean)
barplot(acc.digit, ylim = c(0.8,1), xpd = F, main = "accuracy per digit - test data")
# build confusion matrix
conf.mat = table(y.test, preds.test)
cat("*** confusion matrix : ****\n")
print(conf.mat)

