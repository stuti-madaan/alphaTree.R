library(ggplot2);
library(ROCR);
library(corrplot);
source("atree/alphaTree.R");

hyper <- read.csv("data/allhyper.data")

# data preprocessing
hyper$binary.class <- 1-(hyper$class=="negative."); # The original class was not binary-valued. 
hyper$class <- NULL; # We now use the binarized new class labels.
hyper$TBGInd <- NULL; # No information in this column, which gives an error in "model.matrix".
hyper$TBG <- NULL; # No information in this column, which gives an error in "model.matrix".
hyper <- na.omit(hyper); # We use only the fully-observed rows.
y <- hyper$binary.class; # y (target)
hyper$binary.class <- NULL;
X <- model.matrix(~.-1, hyper); # X (features)

# calculating stats
set.seed(2);
n <- length(y);
random.idx <- sample(1:n, n);
# 50/50 train/test split
train.idx <- random.idx[1:(n/2)];
test.idx <- random.idx[(n/2+1):n];
X.train <- X[train.idx, ];
y.train <- y[train.idx];
X.test <- X[test.idx, ];
y.test <- y[test.idx];

#-------------------------------------------------------------------------------------

n.train <- length(y.train);
pos.train.idx <- which(y.train==1);
neg.train.idx <- which(y.train==0);
bootstrap1.idx <- sample(1:n.train, n.train, replace=TRUE);
bootstrap2.idx <- sample(1:n.train, n.train, replace=TRUE);
bootstrap3.idx <- sample(1:n.train, n.train, replace=TRUE);

# create training datasets
X.train.b1 <- X.train[bootstrap1.idx, ];
y.train.b1 <- y.train[bootstrap1.idx];
X.train.b2 <- X.train[bootstrap2.idx, ];
y.train.b2 <- y.train[bootstrap2.idx];
X.train.b3 <- X.train[bootstrap3.idx, ];
y.train.b3 <- y.train[bootstrap3.idx];

# decision tree options
#   maximum depth = 4
#   mininum number of samples per node = 3
#   class labels = c(1, 0)
option <- list(max.depth=4, min.n=10, class=c(1, 0));

# build decision trees
c45 <- atree(X.train, y.train, 1, option);
c45.b1 <- atree(X.train.b1, y.train.b1, 1, option);
c45.b2 <- atree(X.train.b2, y.train.b2, 1, option);
c45.b3 <- atree(X.train.b3, y.train.b3, 1, option);
alpha.1 <- atree(X.train, y.train, 0, option);
alpha.2 <- atree(X.train, y.train, 2, option);

output.json(c45, "c45");
output.json(c45.b1, "c45.b1");
output.json(c45.b2, "c45.b2");
output.json(c45.b3, "c45.b3");
output.json(alpha.1, "alpha1");
output.json(alpha.2, "alpha2");
#----------------------------------------------------------
# compare decision trees. what are the first and second level splitting features, and how are they different?
#----------------------------------------------------------

# predict
y.c45.pred <- predict(c45, X.test)$minor.class;
y.c45.b1.pred <- predict(c45.b1, X.test)$minor.class;
y.c45.b2.pred <- predict(c45.b2, X.test)$minor.class;
y.c45.b3.pred <- predict(c45.b3, X.test)$minor.class;
y.a1.pred <- predict(alpha.1, X.test)$minor.class;
y.a2.pred <- predict(alpha.2, X.test)$minor.class;
y.c45.bagging.pred <- (y.c45.b1.pred+y.c45.b2.pred+y.c45.b3.pred)/3;
y.eat.pred <- (y.c45.pred+y.a1.pred+y.a2.pred)/3;

# check the correlation among decision trees
pred.df <- data.frame(c45 = y.c45.pred,
						btstrp1 = y.c45.b1.pred,
						btstrp2 = y.c45.b2.pred,
						btstrp3 = y.c45.b3.pred,
						atree1 = y.a1.pred,
						atree2 = y.a2.pred)
corrplot.mixed(cor(pred.df), lower="number", upper="ellipse");

# check AUROC values 
performance(prediction(y.c45.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.b1.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.b2.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.b3.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.a1.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.a2.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.bagging.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.eat.pred, y.test),"auc")@y.values[[1]];
#----------------------------------------------------------
# Question
# report AUROC values, and comment on the results.
#----------------------------------------------------------

# plot ROC curves
plot.df <- NULL;
roc <- performance(prediction(y.c45.pred, y.test),"tpr", "fpr");
plot.df <- rbind(plot.df, data.frame(fpr = roc@x.values[[1]], tpr = roc@y.values[[1]], model="c45"));
roc <- performance(prediction(y.c45.bagging.pred, y.test),"tpr", "fpr");
plot.df <- rbind(plot.df, data.frame(fpr = roc@x.values[[1]], tpr = roc@y.values[[1]], model="bagging"));
roc <- performance(prediction(y.eat.pred, y.test),"tpr", "fpr");
plot.df <- rbind(plot.df, data.frame(fpr = roc@x.values[[1]], tpr = roc@y.values[[1]], model="eat"));

ggplot(plot.df, aes(x=fpr, y=tpr, colour=model)) + 
geom_line() + 
theme_bw();
ggsave("pdfs/allhyper_roc.pdf",width=5,height=3);

ggplot(plot.df, aes(x=fpr, y=tpr, colour=model)) + 
geom_line() + 
theme_bw() + 
coord_cartesian(xlim=c(-0.1,0.25), ylim=c(0.5,1.1));
ggsave("pdfs/allhyper_roc_zoomed.pdf",width=5,height=3);

#----------------------------------------------------------
# Question 
# plot ROC curves, and comment on the results.
#----------------------------------------------------------

# plot ROC curves
plot.df <- NULL;
plot.df <- rbind(plot.df, data.frame(pred=y.c45.pred, label=y.test, model="c45"));
plot.df <- rbind(plot.df, data.frame(pred=y.c45.bagging.pred, label=y.test, model="bagging"));
plot.df <- rbind(plot.df, data.frame(pred=y.eat.pred, label=y.test, model="eat"));

ggplot(plot.df, aes(x=pred, fill=factor(label))) + 
geom_histogram(binwidth=0.05) + 
theme_bw() + 
facet_grid(label~model,scale="free");

ggsave("pdfs/allhyper_density.pdf",width=10,height=3);

#----------------------------------------------------------
# Question 
# plot histograms per model and class, and comment on the results
# specifically, comment on the undersampling decision tree).
#----------------------------------------------------------


