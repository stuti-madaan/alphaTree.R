##################################
# Learn-by-examples
# Author: Yubin Park
# Contact: yubin.park@utexas.edu
# Last Modified: Apr 25th, 2013
##################################

require(ROCR);
source("alphaTree.R");

# a synthetic data example
P <- 1000;
N <- 2000;
synth <- data.frame(x.1 = rnorm(N,0,0.8),x.2 = rnorm(P,1,1), class="pos");
synth <- rbind(synth,data.frame(x.1= rnorm(N,-1,2),x.2=rnorm(N,-1,2),class="neg"));
X <- model.matrix(class~.,data=synth);
y <- as.factor(synth[,"class"]);
model <- grow.atree(X, y, 0.5,4);
minor.class <- model$minor.class;
alpha.pred <- predict.atree(X, model$rules);
rocr.pred <- prediction(alpha.pred$minor.class, (y==minor.class));
rocr.perf <- performance(rocr.pred, "tpr", "fpr");
plot(rocr.perf);


# a real data example in the "rpart" package
require(rpart);
X <- model.matrix(Kyphosis ~ ., data=kyphosis);
y <- kyphosis[,"Kyphosis"];
# $\alpha = 0.5$ and depth=4
model <- grow.atree(X, y, 0.5, 4);
minor.class <- model$minor.class;
alpha.pred <- predict.atree(X, model$rules);
rocr.pred <- prediction(alpha.pred$minor.class, (y==minor.class));
rocr.perf <- performance(rocr.pred, "tpr", "fpr");
plot(rocr.perf); # training ROC 

# $\alpha = 1.0$ and depth=4
model <- grow.atree(X, y, 1.0, 4);
minor.class <- model$minor.class;
alpha.pred <- predict.atree(X, model$rules);
rocr.pred <- prediction(alpha.pred$minor.class, (y==minor.class));
rocr.perf <- performance(rocr.pred, "tpr", "fpr");
plot(rocr.perf); 

# $\alpha = 0.0$ and depth=4
model <- grow.atree(X, y, 0.0, 4);
minor.class <- model$minor.class;
alpha.pred <- predict.atree(X, model$rules);
rocr.pred <- prediction(alpha.pred$minor.class, (y==minor.class));
rocr.perf <- performance(rocr.pred, "tpr", "fpr");
plot(rocr.perf);

# $\alpha = 2.0$ and depth=4
model <- grow.atree(X, y, 2.0, 4);
minor.class <- model$minor.class;
alpha.pred <- predict.atree(X, model$rules);
rocr.pred <- prediction(alpha.pred$minor.class, (y==minor.class));
rocr.perf <- performance(rocr.pred, "tpr", "fpr");
plot(rocr.perf);

# $\alpha = 2.0$ and depth=2
model <- grow.atree(X, y, 2.0, 2);
minor.class <- model$minor.class;
alpha.pred <- predict.atree(X, model$rules);
rocr.pred <- prediction(alpha.pred$minor.class, (y==minor.class));
rocr.perf <- performance(rocr.pred, "tpr", "fpr");
plot(rocr.perf);
print(model);
