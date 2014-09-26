rmda <-
function(X,cls,K=4,model='VEV'){
# Robust Mixture Discriminant Analysis 
# Authors: Charles Bouveyron & Stéphane Girard
# Reference: ???
	
	## Initialization
	C = max(cls)
	
	# Unsupervised part of learning
	clf = Mclust(X,K,ModelNames=model)	# Calling Mclust
	P = clf$z				# Posterior probabilities
	
	## Supervised part of learning (using ML estimation)
	Rinit = runif(C*K)
  #Rinit = c(colSums(P[cls==1,]) / sum(cls==1) * C,1 - (colSums(P[cls==1,]) / sum(cls==1) * C))
  low = rep(0,C*K); up = rep(1,C*K)
  R = solnp(Rinit,fun=.mlefun,eqfun=.eqfun,eqB=rep(1,K),LB=low,UB=up,P=P,cls=cls)$pars
	R = c(colSums(P[cls==1,]) / sum(cls==1) * C,1 - (colSums(P[cls==1,]) / sum(cls==1) * C))
  R = matrix(R,nrow=C,byrow=T)
	#print(R)
	
	## Return the object
	res <- list(K=K, prms=clf, R=R);
	class(res) <- "rmda"
	res
}
