# Negative log-likelihood:
negll.logist<- function(guess, obs.vec, xvals){ # Negative Likelihood model for 2 parameters, the sum of the binomial probabilites for each likelihood
	
	beta0 <- guess[1];
	beta1 <- guess[2];
	p.x   <- 1/(1+ exp(-(beta0+beta1*xvals))) 
	
	llike <- dbinom(x=obs.vec, size=1, prob=p.x, log=TRUE)
	negll <- -sum(llike)
	return(negll)
} 

prof.negll.logist<- function(guess, obs.vec, xvals,beta1){ # Create a single function that looks the same, but the guess is now just beta0. We do this to maximize by changing the value of only one parameter while leaving the other one alone

	beta0 <- guess[1];
	p.x   <- 1/(1+ exp(-(beta0+beta1*xvals)))
	
	llike <- dbinom(x=obs.vec, size=1, prob=p.x, log=TRUE)
	negll <- -sum(llike)
	return(negll)
} 


# Function to simulate data 
logistic.regsim <- function(betas,xs, nreps=10, ntrials=1){
	
	x <- xs; # line not needed, just renaming
	
	beta0 <- betas[1]
	beta1 <- betas[2]
	
	n <- length(xs)
	real.p <- 1/(1+exp(-(beta0+beta1*x))); 
	data.sim <- rbinom(n=nreps, size=ntrials, prob=real.p);
	return(my.data = cbind(x,data.sim))
	
}

# Function to estimate parameters from simulated data
my.logistic.mles <- function(datamat,orig.mles){
	
	my.guess <- orig.mles
	my.data <- datamat
	ml.estim <- optim(par=my.guess, fn=negll.logist, method="Nelder-Mead", obs.vec=my.data[,2], xvals=my.data[,1])
	mles <- ml.estim$par
	return(c(beta0=mles[1], beta1=mles[2]))	

}


###################  Procedural section ############################

#  Simulating data 'on the go', without a function and Wald's CI:

ntrials <- 1; # Binomial with number of trials = 1 is a Bernoulli!!
nreps <- 200;
x <- runif(n=200,min=-3, max=3); # Values of the covariate chosen at random
hist(x);
#Setting P(success) as a function of a covariate
beta0 <- 1.5;
beta1 <- 2.85; # Try lower values and higher values
real.p <- 1/(1+exp(-(beta0+beta1*x)));
plot(x,real.p, pch=16)   # Checking out that simulations make sense
# Simulating data
data.sim <- rbinom(n=nreps, size=ntrials, prob=real.p)
       
# "raw data"
my.data <- cbind(x,data.sim)
colnames(my.data) <- c("covariate", "Successes")

# Computing the neg loglike for ANY two values of beta0 and beta1:
# This is just to see if the negloglike function is working properly
my.guess <- c(0.44,0.6); # Most times the heuristic search algorithms (local searches), can get stuck on a local minimum rather then the overall minumum
# First do this to be able to go inside the llike function and see how it works
guess <- my.guess
obs.vec <- my.data[,2] 
xvals  <- my.data[,1]

# Then test the negative log likelihood function
negll.logist(guess=my.guess, obs.vec=my.data[,2], xvals = my.data[,1])

# Now the optimization itself
# Work with the neg-loglikelihood and then don't need to multiply by -1 the hessian 
ml.estim <- optim(par=my.guess, fn=negll.logist, method="Nelder-Mead", obs.vec=my.data[,2], xvals=my.data[,1], hessian=TRUE)

# Or Work with the log-likelihood and then multiply by -1 the hessian
#ml.estim <- optim(par=my.guess, fn=negll.logist, method="Nelder-Mead",control=list("fnscale"=-1), obs.vec=my.data[,2], xvals=my.data[,1], hessian=TRUE)
mles <- ml.estim$par

#library("numDeriv")
#my.hess <- hessian(func=negll.logist, x=mles, method="Richardson", obs.vec=my.data[,2], xvals=my.data[,1])


############ Wald confidence intervals
library("MASS") # for 'ginv'
my.hess<- ml.estim$hessian 
Fish.Inv <- ginv(my.hess)#solve(my.hess) ### Normally need to multiply by -1 but because we are optimizing the NEGATIVE log like we're ok.
alpha <- 0.05
zalphahalf <- qnorm(p=1-(alpha/2), mean=0, sd=1)
st.errs <-zalphahalf*sqrt(diag(Fish.Inv))
low.cis <- mles - st.errs
hi.cis <- mles + st.errs

CIs.mat <- cbind(low.cis, mles, hi.cis)
colnames(CIs.mat) <- c("2.5%", "MLE", "97.5%")
row.names(CIs.mat) <- c("Beta0", "Beta1")
print(CIs.mat)


# Parametric Bootstrap CI:
# Use the same data as above: data.sim 

beta0.mle <- mles[1]
beta1.mle <- mles[2]
betas.mles <- mles

########## Parametric Bootstrap Confidence Intervals:  
########## Objective is to approximate the samping distribution of the MLES via simulation.

# for a large number of iterations
# (say 2000) do the following
B <- 2000 # number of bootstrap replicates
mlesmat <- matrix(0,nrow=B,ncol=length(betas.mles))


for(i in 1:B){
	# Step 1: Simulate a data set of the same size as the original data
	#         using the original MLEs and the SAME x's
	data.star <- logistic.regsim(betas=betas.mles,xs=x, nreps = nreps, ntrials=1)

	# Step 2: Estimate parameters for each simulated data set just as you did above 
	#         Give the original mles as your starting point for the search
	mles.star <- my.logistic.mles(datamat=data.star,orig.mles=betas.mles)
	
	# Step 3: Save the mles by storing them in a matrix of dimension 2000 rows x 2 columns
	#         Those mles are a sample of size 2000 of the approximate (simulated) sampling 
	#         distribution of the MLEs.  If you do a histogram of these and the original
	#         sample size is large enough, these will look normally distributed
	mlesmat[i,] <- mles.star
}

# Step 4: Compute the 2.5 and the 97.5 percentiles of the simulated sampling distribution of each parameter


par(mfrow=c(1,2))
hist(mlesmat[,1])
abline(v = betas.m)
hist(mlesmat[,2])
abline(v=betas.mles[2])

Boot.cis <- apply(mlesmat,2,FUN=function(x){quantile(x,probs=c(0.025,0.975))})
Boot.cis <- t(Boot.cis) # To put in the same format that Wald CI's matrix above
Boot.cismat <- cbind(Boot.cis[,1],betas.mles,Boot.cis[,2])
colnames(Boot.cismat) <- c("2.5%", "MLE", "97.5%")
row.names(Boot.cismat) <- c("Beta0", "Beta1")
print(Boot.cismat)



########### Profile likelihood confidence interval for beta1 (the slope)

########### Work with the original data

######  Step 1:  select a range of the values of beta1 bracketing the mle for beta1
beta1.values <- seq(from=1.5, to=7.5,by=0.01)
nvals <- length(beta1.values) #301 values
llikes.vec <- rep(0,nvals) # empty vector where we will store the value of the likelihood
						  # score maximized with respect to the beta0's (NOT beta1) Since we already chose what the beta1s will be 
						  # over the selected range of beta1's

# For every value of beta1 in the range 'beta1.values' (these are the values of 'c' 
# from the class notes general CI/LRT lecture) :

for(i in 1:nvals){

	# Fix the value of beta1 to the ith element of the vector beta1.values 
	beta1.c.value <- beta1.values[i]
	
	# Maximize the likelihood ONLY with respect to beta0 (the other parameter)
	# leaving beta1 fixed to the value beta1.c.value.  To do that you must have
	# another likelihood function where the 'guess' consists of only one value,
	# not two.
	guess <- beta0.mle
	prof.min <- optim(par=guess, fn=prof.negll.logist, method="BFGS", obs.vec=data.sim, xvals=x,beta1=beta1.c.value) # BFGS as a search method
	max.lnL <- -prof.min$value	
	llikes.vec[i] <- max.lnL
}

likes.vec <- exp(llikes.vec)
Rel.like <- likes.vec/max(likes.vec)
plot(beta1.values,Rel.like, type="l", col="red", main="Relative profile likelihood for beta 1")
cuttoff <- exp(-qchisq(p=0.95, df=1)/2)
abline(h=cuttoff, col="black", lwd=2)

# Find the beta1.value where the red line crosses the horizontal black line at the cuttoff
# How do we find these crossing points? (For the confidence interval)

rel.like.minus.cuttoff <- Rel.like-cuttoff
where.positive <- which(rel.like.minus.cuttoff > 0, arr.ind=TRUE)
firstNlast.positive <- range(where.positive)

beta1.ci <- beta1.values[firstNlast.positive]
abline(v=beta1.ci[1],col="blue")
abline(v=beta1.ci[2],col="blue")















