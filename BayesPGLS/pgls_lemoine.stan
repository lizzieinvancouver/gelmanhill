data{
	int N; // number of spp 
	int K; // 1 (WTF?)
	vector[N] y; // egg mass 
	matrix[N, N] V; // will be VCV, yes!
	matrix[N, N] Lmat; // matrix full of ones that is the same dimension as the VCV
	vector[N] X; // body mass
}
transformed data{
	real Ndiv; // a real number (holder)
	matrix[N, N] Ident; // a N by N matrix (holder)

	Ndiv <- N; // fill out Ndiv
	Ident <- diag_matrix(rep_vector(1, N)); // matrix of 0s with 1s on diagonal
}
parameters{
	real<lower=0> sigma;
	real<lower=0, upper=1> lambda;
	vector[2] B;
}
transformed parameters{
}
model{
	matrix[N,N] Vlambda; // VCV * lambda; for scaling tree by estimated lambda value
	matrix[N,N] Vsigma; // (VCV*lambda) * sigma; for modeling error according to the lambda estimate
	vector[N] yhat; // holder for predicted values
	real detV; // determinant of the VCV
	real cdf;  // cumulative distribution f(x)??
	real logLike_PGLS; // a log likelihood!

	Vlambda <- (lambda*Lmat + Ident) .* V; //making it so that only the off-diagonals of the VCV are multiplied by lambda
	yhat <- B[1] + B[2]*X; 


	detV <- log_determinant(Vsigma); //log determinant of the Vsigma
	cdf <- ((y-yhat)'*inverse(Vsigma)*(y-yhat)); //not sure why cdf...also, this is identical to Will Pearse's code minus the inverse function used...
	logLike_PGLS <-  -0.5*(detV + cdf); //this is the log-likelihood function defined in Revell 2010, with the exception of the + nlog(2pi)
	increment_log_prob(logLike_PGLS); //estimating the log likelihood with some function from stan that is more efficient (??)
	
	y ~ multi_normal(yhat, Vsigma);
	
	B ~ normal(0, 1); //why not multi-normal for B, model relationship between the two?
	sigma ~ cauchy(0, 2.5); //okay!
	lambda ~ beta(1, 1); //okay, standard
}
