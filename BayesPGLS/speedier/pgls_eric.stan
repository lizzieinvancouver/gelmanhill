data{
	int N; // number of species
	vector[N] y; // egg mass 
	vector[N] X; // body mass
	
	int nedge;
	vector[nedge] edgelen;
	int des[nedge];
	int anc[nedge];
	int externalEdge[N];
	int desexternalEdge[N];
	vector[nedge+1] distFromRoot;
}
parameters{
	real<lower=0> sigma;
	real<lower=0, upper=1> lambda;
	vector[2] B;
}
model{
	vector[N] yhat; // holder for predicted values
	real detV; // log determinant of the VCV
	real dev;  // (Y-XB)'(VCV)^-1(Y-XB)
	real logLike_PGLS; // a log likelihood

	int anc_edge;
	int des_edge;
	real len;
	real pA;
	vector[nedge+1] p;
	vector[nedge+1] Yhat;
	vector[nedge+1] YY;

	real tempY;
	real tempp;
	real tempYY;
	vector[nedge+1] logdet;
	real tempdet;
	vector[nedge] tedgelen;
	vector[nedge] tempedge;
	tedgelen <- edgelen;
	tempedge <- tedgelen * lambda;
	tedgelen <- tempedge;
  tempedge[externalEdge] <- tedgelen[externalEdge] + (1 - lambda) * distFromRoot[desexternalEdge];
  tedgelen <- tempedge;
	
	p <- rep_vector(0,nedge+1);
	Yhat <- rep_vector(0,nedge+1);
	YY <- rep_vector(0,nedge+1);
	logdet <- rep_vector(0,nedge+1);


	yhat <- y - (B[1] + B[2]*X); //model

	for(i in 1:nedge)
	{
	  anc_edge <- anc[i];
    des_edge <- des[i];
	  len <- tedgelen[i]*sigma^2;
	  
	  if(des_edge<=N)
	  {
	    p[des_edge] <- 1/len;
	    Yhat[des_edge] <- yhat[des_edge];
	    logdet[des_edge] <- log(len);
	    YY[des_edge] <- yhat[des_edge]^2/len;
	  } else
	  {
	    pA <- p[des_edge];
	    tempY <- Yhat[des_edge]/pA;
	    Yhat[des_edge] <- tempY;

	    p[des_edge] <- pA/(1+len*pA);
	    tempdet <- logdet[des_edge] + log(1+len*pA);
	    logdet[des_edge] <- tempdet;
	    
	    tempYY <- YY[des_edge] - (len*pA^2)/(1+len*pA)*Yhat[des_edge]*Yhat[des_edge];
	    YY[des_edge] <- tempYY;
	  }
	  tempp <- p[anc_edge] + p[des_edge];
	  p[anc_edge] <- tempp;
	  
	  tempY <- Yhat[anc_edge] + Yhat[des_edge]*p[des_edge];
	  Yhat[anc_edge] <- tempY;

	  tempdet <- logdet[anc_edge] + logdet[des_edge];
	  logdet[anc_edge] <- tempdet;
	  
	  tempYY <- YY[anc_edge] + YY[des_edge];
	  YY[anc_edge] <- tempYY;
	}
	
	detV <- logdet[anc_edge]; //log determinant of the Vsigma
	dev <- YY[anc_edge]; //(Y-XB)'(Vsigma)^-1(Y-XB)
	logLike_PGLS <-  -0.5*(detV + dev); //multivariate log-likelihood without constants
	increment_log_prob(logLike_PGLS);

	B ~ normal(0, 1); //why not multi-normal for B, model relationship between the two?
	sigma ~ cauchy(0, 2.5); //okay!
	lambda ~ beta(1, 1); //okay, standard
}