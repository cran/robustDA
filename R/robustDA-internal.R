.eqfun <-
function(R,P,cls){
  K = ncol(P)
  C = max(cls)
  R = matrix(R,nrow=C,byrow=T)
  colSums(R)
}
.mlefun <-
function(R,P,cls){
# MLE function
  #cat('.')
	K = ncol(P)
	C = max(cls)
  R = matrix(R,nrow=C,byrow=T)
	f = 0
	for(i in 1:C){
		Ri = R[i,]
    ni = sum(cls==i)
		f = f + sum(log(P[cls==i,] * matrix(1,ni,1)%*%Ri));
	}
	f
}