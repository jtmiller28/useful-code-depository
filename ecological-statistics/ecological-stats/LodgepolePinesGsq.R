### Is the Poisson Distribution a good distribution to Explain the Lodge Pole Pine Distribution?

#  Count frequencies are entered into the vector yy here.  The last frequency
#  is the pooled tail counts.
yy=c(7,16,20,24,17,9,5,2); # 7 quadrats with 0 trees, 16 quadrates with 1 trees, ...Allows us to model in a multinomial distribution

#  Data in example are from:  

#  Initial parameter value is calculated from the approximate sample mean.
kk=length(yy);
nn=sum(yy);
xx=0:(kk-1);
lambda0=sum(xx*yy)/nn;

#  ML objective function "negloglike.ml" is negative of log-likelihood;
#  the optimization routine in R, "optim", is a minimization
#  routine.  The two function arguments are:  theta = vector of
#  parameters (transformed to real line), ys = vector of frequencies.

negloglike.ml=function(theta,ys)  
{
   lambda=exp(theta);     #  Constrains 0 < lambda, otherwise NaNs are produced. # Mean number of trees per unit area
   k=length(ys);
   x=0:(k-1);
   x1=x[1:(k-1)];
   p=rep(0,k);
   p[1:(k-1)]=exp(-lambda+x1*log(lambda)-lfactorial(x1)); # The log(Poisson Probability) same as dpois(x=x1, lamba=lambda)
   p[k]=1-sum(p[1:(k-1)]);
   ofn=-sum(ys*log(p));     #  No need to calculate all the factorials.
   return(ofn);
}

# The ML estimate.
MULTML=optim(par=log(lambda0),
   negloglike.ml,NULL,method="BFGS",ys=yy);  #  Nelder-Mead algorithm is not
                                             #  reliable for 1-D problems.
reslts=c(exp(MULTML$par[1]),-MULTML$val); # Take the theta, and then transform via exponentiation to a parameter that makes sense for the probability distribution (we do the same as the exp above)
lambda.ml=reslts[1];            # This is the ML estimate.
nn=sum(yy);
loglike.ml=reslts[2]+lfactorial(nn)-sum(lfactorial(yy)); #  Log-likelihood.

# Calculate expected values, LR statistic, etc.
xx1=xx[1:(kk-1)];
pp=rep(0,kk);
pp[1:(kk-1)]=exp(-lambda.ml+xx1*log(lambda.ml)-lfactorial(xx1));
pp[kk]=1-sum(pp[1:(kk-1)]);
EE=nn*pp;
y1=yy;
y1[y1==0]=1;               # Guard against log(0) in G-squared.
Gsq=2*sum(yy*log(y1/EE));  # G-squared goodness of fit statistic.
pvalG=1-pchisq(Gsq,8-1-1);     # p-value (chisquare distribution) for G-squared.
Xsq=sum((yy-EE)^2/EE);     # Pearson goodness of fit statistic.
pvalX=1-pchisq(Xsq,8-1-1);     # p-value (chisquare distribution) for Pearson

#  Print the results.
lambda.ml;
loglike.ml;
Gsq;
pvalG;
Xsq;
pvalX;
cbind(EE,yy);


> lambda.ml;
[1] 2.859631
> loglike.ml;
[1] -13.97714
> Gsq;
[1] 1.276895
> pvalG;
[1] 0.9729164
> Xsq;
[1] 1.260605
> pvalX;
[1] 0.9737855
> cbind(EE,yy);
            EE yy
[1,]  5.728991  7
[2,] 16.382799 16
[3,] 23.424378 20
[4,] 22.328357 24
[5,] 15.962714 17
[6,]  9.129494  9
[7,]  4.351164  5
[8,]  2.692104  2

source("AbundanceToolkit.R")
all.fits <- abund.fit(y1 = yy, names="Lodgepole Pines", "Plot counts")




