
alpha.div <- function(c1, c2, alpha){

	adiv <- -Inf;
	if(alpha==1.0){ adiv <- sum(log(c1/c2)*c1); }
	else if(alpha==0.0){ adiv <- sum(log(c2/c1)*c2);	}
	else{ adiv <- (1.0-sum((c1^(alpha)) * (c2^(1-alpha))))/alpha/(1-alpha);}
	
	if(adiv <= 0.0){ adiv <- 0.0; }

	return(adiv);
}

alpha.gain <- function(x, y, alpha, y.lst){
	
	smoother <- 1e-5;
	max.gain <- -Inf;
	max.value <- NA;
	x.lst <- unique(x);
	
	if(length(x.lst) > 30){
		width <- (max(x)-min(x))/30;
		x.lst <- c(seq(from=min(x), to=max(x), by=width));
	}
	
	if(length(x.lst)==2){ x.lst <- x.lst[1]; }
	
	for(split in x.lst){
		y.left <- y[which(x <= split)];
		y.right <- y[which(x > split)];
		n.y <- length(y);
		n.y.left <- length(y.left);
		n.y.right <- length(y.right);
		n.pos <- sum(y==y.lst[1]);
		n.pos.left <- sum(y.left==y.lst[1]);
		n.pos.right <- sum(y.right==y.lst[1]);
		node.0 <- c((n.y-n.pos),n.pos);
		node.l <- c((n.y.left - n.pos.left),n.pos.left);
		node.r <- c((n.y.right - n.pos.right),n.pos.right);
		c.0 <- node.0+smoother;
		c.l <- node.l+smoother;
		c.r <- node.r+smoother;
		c.0 <- c.0/sum(c.0);
		c.l <- c.l/sum(c.l);
		c.r <- c.r/sum(c.r);
		p.l <- length(y.left)/length(y);
		p.r <- length(y.right)/length(y);
		if(length(y.left) < 3 | length(y.right) < 3 ) next;
		gain <- p.r*alpha.div(c.r, c.0, alpha) + p.l*alpha.div(c.l, c.0, alpha);
		if(gain > max.gain){
			max.gain <- gain;
			max.value <- split;
		}
	}
	return(list(value=max.value, gain=max.gain));
}


split.variable <- function(X, y, alpha, y.lst){
	M <- ncol(X);
	N <- nrow(X);
	max.gain <- -Inf;
	max.feature <- NA;
	max.value <- NA;
	
	if(N < 3 || class.ratio(y, y.lst)==0 || class.ratio(y, y.lst)==1)
	{ return(list(gain=max.gain, feature=max.feature, value=max.value)); }
	
	for(i in 1:M){
		if(length(unique(X[,i]))==1) next;
		ag <- alpha.gain(X[,i], y, alpha, y.lst);
		if(max.gain < ag$gain){
			max.gain <- ag$gain;
			max.feature <- i;
			max.value <- ag$value;
		}
	}
	return(list(gain=max.gain, feature=max.feature, value=max.value));
}

append.rule <- function(rule,var,value,op){
	newrule = ""
	if(rule==""){ newrule=paste(var,op,value, sep=""); }
	else{ newrule=paste(rule," & ", var, op, value, sep="");}
	return(newrule)
}

class.ratio <- function(y, y.lst){
	ratio <- mean(as.numeric(y==y.lst[1]));
	return(ratio)
}

grow.atree <- function(X, y, alpha, depth){

	N <- length(y); # a total number of data samples
	pid.lst <- rep(1,N); # a list of partition indeces
	rules <- data.frame(pid=1,rule="",pred=0.0);
	y.lst <- sort(unique(y),decreasing=TRUE);

	for(d in 1:depth){

		n.pid <- length(unique(pid.lst));
		rules.tmp <- NULL;
		pid.lst.tmp <- rep(1,N);
		pid.new <- 1;
		
		for(pid.old in 1:n.pid){

			sub.idx <- which(pid.lst==pid.old);
			X.sub <- X[sub.idx,];
			y.sub <- y[sub.idx];
			
			split.result <- split.variable(X.sub, y.sub, alpha, y.lst);	
			feature <- split.result$feature;
			value <- split.result$value;
			if(is.na(feature)){ # No splitting variable
				pid.lst.tmp[sub.idx] <- pid.new;
				rules.tmp <- rbind(rules.tmp, 
									data.frame(pid=pid.new, 
									rule=rules[pid.old,"rule"],
									pred=rules[pid.old,"pred"]));
				pid.new <- pid.new + 1;				
			}else{
				r.var <- as.character(colnames(X)[feature]);
				r.value <- as.character(value);
				child1.idx <- sub.idx[which(X.sub[,feature] <= value)];
				child2.idx <- sub.idx[which(X.sub[,feature] > value)];
				pid.lst.tmp[child1.idx] <- pid.new;
				rules.tmp <- rbind(rules.tmp,
					data.frame(pid=pid.new, 
					rule=append.rule(rules[pid.old,2],r.var,r.value,"<="),
					pred=class.ratio(y[child1.idx],y.lst)));
				pid.new <- pid.new + 1;
				pid.lst.tmp[child2.idx] <- pid.new;
				rules.tmp <- rbind(rules.tmp,
					data.frame(pid=pid.new, 
					rule=append.rule(rules[pid.old,2],r.var,r.value,">"),
					pred=class.ratio(y[child2.idx],y.lst)));
				pid.new <- pid.new + 1;
			}
		}
		pid.lst <- pid.lst.tmp;
		rules <- rules.tmp;
	}
	return(list(positive=y.lst[1],rules=rules));
}

predict.atree <- function(newdata, rules){
	df <- as.data.frame(newdata);
	n.rules <- nrow(rules);
	N <- nrow(newdata);
	pred <- rep(0,N);
	for(i in 1:n.rules){
		idx <- eval(parse(text=as.character(rules[i,"rule"])),envir=df);
		pred[idx] <- rules[i,"pred"]
	}
	return(pred)
}