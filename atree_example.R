# example 1: build alpha trees with different alphas

library(ROCR);
source("atree/alphaTree.R");

pima <- read.csv('data/pima-indians-diabetes.data',header=F)
colnames(pima) <- c("n.preg","plasma.glucose","diastolic.bp",
					"triceps.thickness","serum.insulin",
					"bmi","pedigree","age","class");

y <- pima[,ncol(pima)]
X <- pima[,1:(ncol(pima)-1)]

option <- list(max.depth=4);
atfit <- atree(X, y, 1, option)
output.json(atfit);
perf <- performance(prediction(predict(atfit, X)$minor.class, y),"tpr","fpr")
plot(perf)

atfit <- atree(X, y, 4, option)
output.json(atfit);
perf <- performance(prediction(predict(atfit, X)$minor.class, y),"tpr","fpr")
plot(perf)


# properties of alpha-tree

true <- rep("Non-diabetic",length(y));
true[y >  0] <- "Diabetic";
plot <- NULL;
option <- list(max.depth=1);

atfit <- atree(X[,c("plasma.glucose"),drop=F], y, 0, option);
y.pred <- predict(atfit, X[,c("plasma.glucose"),drop=F])$minor.class;
pred <- rep("Non-diabetic", length(y));
pred[which(y.pred > min(y.pred))] <- "Diabetic"
plot <- rbind(plot, data.frame(plasma.glucose=X$plasma.glucose, pred=pred, true=true, alpha="alpha=0"));

atfit <- atree(X[,c("plasma.glucose"),drop=F], y, 1, option);
y.pred <- predict(atfit, X[,c("plasma.glucose"),drop=F])$minor.class;
pred <- rep("Non-diabetic", length(y));
pred[which(y.pred > min(y.pred))] <- "Diabetic"
plot <- rbind(plot, data.frame(plasma.glucose=X$plasma.glucose, pred=pred, true=true, alpha="alpha=1"));

atfit <- atree(X[,c("plasma.glucose"),drop=F], y, 4, option);
y.pred <- predict(atfit, X[,c("plasma.glucose"),drop=F])$minor.class;
pred <- rep("Non-diabetic", length(y));
pred[which(y.pred > min(y.pred))] <- "Diabetic"
plot <- rbind(plot, data.frame(plasma.glucose=X$plasma.glucose, pred=pred, true=true, alpha="alpha=4"));

atfit <- atree(X[,c("plasma.glucose"),drop=F], y, 32, option);
y.pred <- predict(atfit, X[,c("plasma.glucose"),drop=F])$minor.class;
pred <- rep("Non-diabetic", length(y));
pred[which(y.pred > min(y.pred))] <- "Diabetic"
plot <- rbind(plot, data.frame(plasma.glucose=X$plasma.glucose, pred=pred, true=true, alpha="alpha=32"));

library(ggplot2)

ggplot(plot, aes(x=plasma.glucose, fill=pred)) + 
geom_histogram(binwidth=5) +
facet_grid(alpha~true) + 
theme_bw()

ggsave("pdfs/atree_properties.pdf",width=8,height=6)






