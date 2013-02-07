require(ggplot2);
require(reshape);
source("alphaTree.R");

# generate synthetic data
P <- 1000;
N <- 2000;
synth <- data.frame(x.1 = rnorm(N,0,0.8),x.2 = rnorm(P,1,1), class="pos");
synth <- rbind(synth,data.frame(x.1= rnorm(N,-1,2),x.2=rnorm(N,-1,2),class="neg"));
X <- model.matrix(class~.,data=synth);
y <- as.factor(synth[,"class"]);

#-----------------------------------------------------------------------------------
ggplot(data=synth,aes(x=x.1,y=x.2,colour=class))+theme_bw()+geom_point(size=1)+geom_vline(xintercept=0)+geom_hline(yintercept=0)+scale_colour_brewer(palette="Set1");
ggsave("synthetic_data.png",width=4,height=3)


#-----------------------------------------------------------------------------------
# two alpha-trees with different alpha values
model <- grow.atree(X,y,0,1,F);
pred.0 <- predict.atree(X,model$rules);
model <- grow.atree(X,y,0.5,1,F);
pred.0.5 <- predict.atree(X,model$rules);
model <- grow.atree(X,y,1,1,F);
pred.1 <- predict.atree(X,model$rules);
# compare the results from different alpha values
data <- data.frame(x.1=X[,2],x.2=X[,3],
					alpha.0=pred.0,
					alpha.0.5=pred.0.5,
					alpha.1=pred.1,
					class=y);
data.l <- melt(data,id=c("x.1","x.2","class"));
colnames(data.l) <- c("x.1","x.2","class","alpha","pred")
ggplot(data=data.l,aes(x=x.1,y=x.2,colour=pred))+theme_bw()+geom_point(size=1)+geom_vline(xintercept=0)+geom_hline(yintercept=0)+facet_grid(class~alpha);
ggsave("first_cut.png",width=6,height=3)

#-----------------------------------------------------------------------------------
# two alpha-trees with different alpha values
model <- grow.atree(X,y,0,2,F);
pred.0 <- predict.atree(X,model$rules);
model <- grow.atree(X,y,0.5,2,F);
pred.0.5 <- predict.atree(X,model$rules);
model <- grow.atree(X,y,1,2,F);
pred.1 <- predict.atree(X,model$rules);
# compare the results from different alpha values
data <- data.frame(x.1=X[,2],x.2=X[,3],
					alpha.0=pred.0,
					alpha.0.5=pred.0.5,
					alpha.1=pred.1,
					class=y);
data.l <- melt(data,id=c("x.1","x.2","class"));
colnames(data.l) <- c("x.1","x.2","class","alpha","pred")
ggplot(data=data.l,aes(x=x.1,y=x.2,colour=pred))+theme_bw()+geom_point(size=1)+geom_vline(xintercept=0)+geom_hline(yintercept=0)+facet_grid(class~alpha);
ggsave("second_cut.png",width=6,height=3)

