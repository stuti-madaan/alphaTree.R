# example 2: ensemble alpha trees to improve prediction

library(ROCR);
source("atree/alphaTree.R");
pima <- read.csv('data/pima-indians-diabetes.data',header=F)
colnames(pima) <- c("n.preg","plasma.glucose","diastolic.bp",
					"triceps.thickness","serum.insulin",
					"bmi","pedigree","age","class");
n <- nrow(pima);
m <- ncol(pima);
set.seed(1234)
pima.shuffle <- pima[sample(1:n,n),];
pima.train <- pima.shuffle[1:(n/5*4),];
pima.test <- pima.shuffle[(n/5*4+1):n,];
y.train <- pima.train[,m];
X.train <- pima.train[,1:(m-1)];
y.test <- pima.test[,m];
X.test <- pima.test[,1:(m-1)];
option <- list(max.depth=10);


# a single C4.5
c45 <- atree(X.train, y.train, 1, option);
perf <- performance(prediction(predict(c45, X.test)$minor.class, y.test),"tpr","fpr");
plot <- data.frame(fpr=attr(perf,"x.values")[[1]], 
					tpr=attr(perf,"y.values")[[1]], 
					model="single C4.5");

# ensemble three alpha trees
eat <- atree(X.train, y.train, 1, option);
eat <- ensemble(eat, atree(X.train, y.train, 2, option));
eat <- ensemble(eat, atree(X.train, y.train, 0, option));
perf <- performance(prediction(predict(eat, X.test)$minor.class, y.test),"tpr","fpr");
plot <- rbind(plot, data.frame(fpr=attr(perf,"x.values")[[1]], 
					tpr=attr(perf,"y.values")[[1]], 
					model="3 Alpha-Trees"));

# ensemble more alpha trees
eat <- ensemble(eat, atree(X.train, y.train, 0.5, option));
eat <- ensemble(eat, atree(X.train, y.train, 1.5, option));
eat <- ensemble(eat, atree(X.train, y.train, 2.5, option));
perf <- performance(prediction(predict(eat, X.test)$minor.class, y.test),"tpr","fpr");
plot <- rbind(plot, data.frame(fpr=attr(perf,"x.values")[[1]], 
					tpr=attr(perf,"y.values")[[1]], 
					model="6 Alpha-Trees"));

library(ggplot2)
ggplot(plot, aes(x=fpr, y=tpr, colour=model, linetype=model)) + 
geom_path(size=1) +
theme_bw() + 
theme(legend.position=c(0.7,0.3))
ggsave("pdfs/beat_rocs.pdf",width=4, height=3);





# bootstrap estimation
set.seed(1234);
plot <- NULL;
for( i in 1:30 ){
	pima.shuffle <- pima[sample(1:n,n),];
	pima.train <- pima.shuffle[1:(n/5*4),];
	pima.test <- pima.shuffle[(n/5*4+1):n,];
	y.train <- pima.train[,m];
	X.train <- pima.train[,1:(m-1)];
	y.test <- pima.test[,m];
	X.test <- pima.test[,1:(m-1)];
	option <- list(max.depth=10);


	# a single C4.5
	c45 <- atree(X.train, y.train, 1, option);
	perf <- performance(prediction(predict(c45, X.test)$minor.class, y.test),"auc");
	plot <- rbind(plot, data.frame(auc=attr(perf,"y.values")[[1]], 
					model="single C4.5"));

	# ensemble three alpha trees
	eat <- atree(X.train, y.train, 1, option);
	eat <- ensemble(eat, atree(X.train, y.train, 2, option));
	eat <- ensemble(eat, atree(X.train, y.train, 0, option));
	perf <- performance(prediction(predict(eat, X.test)$minor.class, y.test),"auc");
	plot <- rbind(plot, data.frame(auc=attr(perf,"y.values")[[1]], 
					model="3 Alpha-Trees"));

	# ensemble more alpha trees
	eat <- ensemble(eat, atree(X.train, y.train, 0.5, option));
	eat <- ensemble(eat, atree(X.train, y.train, 1.5, option));
	eat <- ensemble(eat, atree(X.train, y.train, 2.5, option));
	perf <- performance(prediction(predict(eat, X.test)$minor.class, y.test),"auc");
	plot <- rbind(plot, data.frame(auc=attr(perf,"y.values")[[1]], 
					model="6 Alpha-Trees"));
}

library(ggplot2)
ggplot(plot, aes(x=model, y=auc, fill=model)) + 
geom_boxplot() +
theme_bw() + 
theme(legend.position="none")
ggsave("pdfs/beat_aucs.pdf",width=4, height=3);





