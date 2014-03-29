
library(ROCR);
library(corrplot);
source("atree/alphaTree.R");

hyper <- read.csv("data/allhyper.data")

# data preprocessing
hyper$binary.class <- 1-(hyper$class=="negative.");
hyper$class <- NULL;
hyper$TBGInd <- NULL;
hyper$TBG <- NULL;
hyper <- na.omit(hyper);
y <- hyper$binary.class;
hyper$binary.class <- NULL;
X <- model.matrix(~.-1, hyper);

# calculating stats
set.seed(2);
n <- length(y);
random.idx <- sample(1:n, n);
# 80/20 train/test split
train.idx <- random.idx[1:(n/2)];
test.idx <- random.idx[(n/2+1):n];
X.train <- X[train.idx, ];
y.train <- y[train.idx];
X.test <- X[test.idx, ];
y.test <- y[test.idx];
n.train <- length(y.train);
pos.train.idx <- which(y.train==1);
neg.train.idx <- which(y.train==0);
bootstrap1.idx <- sample(1:n.train, n.train, replace=TRUE);
bootstrap2.idx <- sample(1:n.train, n.train, replace=TRUE);
bootstrap3.idx <- sample(1:n.train, n.train, replace=TRUE);
oversample.idx <- c(neg.train.idx, sample(pos.train.idx, length(neg.train.idx), replace=TRUE));
#----------------------------------------------------------
# Question a)
# undersample.idx <- "generate undersampling indecies"
#----------------------------------------------------------

# create training datasets
X.train.os <- X.train[oversample.idx, ];
y.train.os <- y.train[oversample.idx];
X.train.us <- X.train[undersample.idx, ];
y.train.us <- y.train[undersample.idx];
X.train.b1 <- X.train[bootstrap1.idx, ];
y.train.b1 <- y.train[bootstrap1.idx];
X.train.b2 <- X.train[bootstrap2.idx, ];
y.train.b2 <- y.train[bootstrap2.idx];
X.train.b3 <- X.train[bootstrap3.idx, ];
y.train.b3 <- y.train[bootstrap3.idx];

# decisino tree options
option <- list(max.depth=10, min.n=3, class=c(1, 0));

# build decision trees
c45 <- atree(X.train, y.train, 1, option);
c45.os <- atree(X.train.os, y.train.os, 1, option);
c45.us <- atree(X.train.us, y.train.us, 1, option);
c45.b1 <- atree(X.train.b1, y.train.b1, 1, option);
c45.b2 <- atree(X.train.b2, y.train.b2, 1, option);
c45.b3 <- atree(X.train.b3, y.train.b3, 1, option);
alpha.1 <- atree(X.train, y.train, 0, option);
#----------------------------------------------------------
# Question b)
# alpha.2 <- "build an alpha-tree with alpha=2"
#----------------------------------------------------------

# predict
y.c45.pred <- predict(c45, X.test)$minor.class;
y.c45.os.pred <- predict(c45.os, X.test)$minor.class;
y.c45.us.pred <- predict(c45.us, X.test)$minor.class;
y.c45.b1.pred <- predict(c45.b1, X.test)$minor.class;
y.c45.b2.pred <- predict(c45.b2, X.test)$minor.class;
y.c45.b3.pred <- predict(c45.b3, X.test)$minor.class;
y.a1.pred <- predict(alpha.1, X.test)$minor.class;
y.a2.pred <- predict(alpha.2, X.test)$minor.class;
y.c45.bagging.pred <- (y.c45.b1.pred+y.c45.b2.pred+y.c45.b3.pred)/3;
#----------------------------------------------------------
# Question c)
# y.eat.pred <- "combine 1) y.c45.pred, 2) y.a1.pred, and 3) y.a2.pred to construct an EAT prediction vector"
#----------------------------------------------------------

# check the correlation among decision trees
pred.df <- data.frame(c45 = y.c45.pred,
					  	ovs = y.c45.os.pred,
						uds = y.c45.us.pred,
						btstrp1 = y.c45.b1.pred,
						btstrp2 = y.c45.b2.pred,
						btstrp3 = y.c45.b3.pred,
						atree1 = y.a1.pred,
						atree2 = y.a2.pred)
corrplot.mixed(cor(pred.df),lower="number",upper="ellipse");

# check AUROC values 
performance(prediction(y.c45.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.os.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.us.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.b1.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.b2.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.b3.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.a1.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.a2.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.c45.bagging.pred, y.test),"auc")@y.values[[1]];
performance(prediction(y.eat.pred, y.test),"auc")@y.values[[1]];
#----------------------------------------------------------
# Question d)
# report AUROC values, and comment on the results.
#----------------------------------------------------------

# plot ROC curves
plot.df <- NULL;
roc <- performance(prediction(y.c45.pred, y.test),"tpr", "fpr");
plot.df <- rbind(plot.df, data.frame(fpr = roc@x.values[[1]], tpr = roc@y.values[[1]], model="c45"));
roc <- performance(prediction(y.c45.os.pred, y.test),"tpr", "fpr");
plot.df <- rbind(plot.df, data.frame(fpr = roc@x.values[[1]], tpr = roc@y.values[[1]], model="oversample"));
roc <- performance(prediction(y.c45.us.pred, y.test),"tpr", "fpr");
plot.df <- rbind(plot.df, data.frame(fpr = roc@x.values[[1]], tpr = roc@y.values[[1]], model="undersample"));
roc <- performance(prediction(y.a1.pred, y.test),"tpr", "fpr");
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
# Question e)
# plot ROC curves, and comment on the results.
#----------------------------------------------------------
