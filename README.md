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

model <- grow.atree(X,y,0,5);

pred.0 <- predict.atree(X,model$rules);

model <- grow.atree(X,y,1.0,5);

pred.1 <- predict.atree(X,model$rules);

data <- data.frame(id=1:length(y),alpha.0=pred.0,alpha.1=pred.1,class=y);

ggplot(data=data,aes(x=alpha.0,y=alpha.1,colour=class))+
theme_bw()+
scale_colour_brewer(palette="Set1")+
geom_jitter()+
geom_rug()+
geom_abline(intercept=0,slope=1);
