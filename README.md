alphaTree.R
===========
Description: A R version of alpha-Tree.


Example code:

require(ggplot2);

source("alphaTree.R");

N <- 500;

synth <- data.frame(f1 = rnorm(N,1,1),
                    f2 = rnorm(N,1,1),
                    f3 = rnorm(N,1,1), 
                    class="0");

synth <- rbind(synth,data.frame(f1= rnorm(N,-1,1),
                                f2=rnorm(N,-1,1),
                                f3=rnorm(N,-1,1), 
                                class="1"));

X <- model.matrix(class~.,data=synth);

y <- as.factor(synth[,"class"]);

model <- grow.atree(X,y,alpha=0.5,depth=5);

