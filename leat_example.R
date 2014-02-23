# example 3: lift-boosting ensemble

library(ROCR);
source("atree/alphaTree.R");

pima <- read.csv('data/pima-indians-diabetes.data',header=F)
colnames(pima) <- c("n.preg","plasma.glucose","diastolic.bp",
					"triceps.thickness","serum.insulin",
					"bmi","pedigree","age","class");

y <- pima[,ncol(pima)]
X <- pima[,1:(ncol(pima)-1)]

plot <- NULL;
plot <- rbind(plot, data.frame(lift=1,cov=1,model="C4.5"))
plot <- rbind(plot, data.frame(lift=1,cov=1,model="AT w Lift Stopping"))
plot <- rbind(plot, data.frame(lift=1,cov=1,model="LEAT (3 AT)"))
plot <- rbind(plot, data.frame(lift=1,cov=1,model="LEAT (6 AT)"))

for(lift in seq(from=1.2, to=2.4, by=0.1)){

option <- list(max.depth=5);
atfit <- atree(X, y, 1, option)
aleat <- ext.highlift(atfit, lift);
plot <- rbind(plot, data.frame(lift=lift, 
					cov=sum(predict.leat(aleat, X))/nrow(X),
					model="C4.5"))

option <- list(max.depth=5, lift=lift);
atfit <- atree(X, y, 1, option)
aleat <- ext.highlift(atfit, lift);
plot <- rbind(plot, data.frame(lift=lift, 
					cov=sum(predict.leat(aleat, X))/nrow(X),
					model="AT w Lift Stopping"))
					
option <- list(max.depth=5, lift=lift);
atfit <- atree(X, y, 1, option)
leat <- ext.highlift(atfit, lift);
atfit <- atree(X, y, 0, option)
leat <- unique.ensemble(leat, ext.highlift(atfit, lift));
atfit <- atree(X, y, 2, option)
leat <- unique.ensemble(leat, ext.highlift(atfit, lift));
plot <- rbind(plot, data.frame(lift=lift, 
					cov=sum(predict.leat(leat, X))/nrow(X),
					model="LEAT (3 AT)"))
					
atfit <- atree(X, y, 4, option)
leat <- unique.ensemble(leat, ext.highlift(atfit, lift));
atfit <- atree(X, y, 16, option)
leat <- unique.ensemble(leat, ext.highlift(atfit, lift));
atfit <- atree(X, y, 32, option)
leat <- unique.ensemble(leat, ext.highlift(atfit, lift));
plot <- rbind(plot, data.frame(lift=lift, 
					cov=sum(predict.leat(leat, X))/nrow(X),
					model="LEAT (6 AT)"))
}


library(ggplot2)

ggplot(plot, aes(x=cov, y=lift, colour=model, linetype=model)) + 
geom_path(size=1) + 
theme_bw() + 
theme(legend.position=c(0.8,0.6))

ggsave("pdfs/leat_liftc.pdf",width=8, height=6)







