# Example of a likelihood ratio test and a Parametric Bootstrap likelihood ratio test (PBLRT):

# Suppose we have discrete data which are the number of 
# successes in a series of N=10 trials
# The trials were repeated under 3 different experimental conditions.
# Under the first condition it was repeated n1 times
# under the second condition  n2 times and
# under the third condition n3 times.
# we wish to test the null 
# Ho: p1 = p2 = p3 = p vs. the alternative # The probability of success is the same across conditions 
# H1: p1 !=p2 != p3 # is not equal

# Here's the data 
N <- 10
n1 <- 8;n2 <- 6;n3 <- 15;
#set.seed(31011975)
samp1 <- c(4,7,8,6,5,8,5,4); #rbinom(n=n1, size=N, prob=0.69); # Gives number of successes out of 10 trials
samp2 <- c(5,7,6,8,8,8); #rbinom(n=n2, size=N, prob=0.72);
samp3 <- c(6,8,7,7,7,8,6,7,7,7,6,5,6,5,6);  #rbinom(n=n3, size=N, prob=0.65);


# Likelihood function under the null
lnLo <- function(counts1,counts2,counts3,Ntrials){
	
	all.counts <- c(counts1,counts2, counts3) # pooled 
	p.hat <- sum(all.counts)/(Ntrials*length(all.counts)) # the sum of the mles, The proportion of successes # For poisson its the average therefore the *sample mean*
	llikevec <- dbinom(x=all.counts, size=Ntrials, prob=p.hat, log=TRUE) # log-likelihood maximized under the Null # For poisson use dpios
	return(sum(llikevec))
}

# Likelihood function under the alternative
lnL1 <- function(counts1,counts2,counts3,Ntrials){
	
	phat1 <- sum(counts1)/(Ntrials*length(counts1)) # Take the proportion of successes per 
	phat2 <- sum(counts2)/(Ntrials*length(counts2))
	phat3 <- sum(counts3)/(Ntrials*length(counts3))	
	
	llike1 <- sum(dbinom(x=counts1,size=Ntrials, prob=phat1, log=TRUE))
	llike2 <- sum(dbinom(x=counts2,size=Ntrials, prob=phat2, log=TRUE))
	llike3 <- sum(dbinom(x=counts3,size=Ntrials, prob=phat3, log=TRUE))
	
	return(sum(c(llike1,llike2,llike3)))
			
}

# Computing Gsq = -2log(Lo/L1)
lnLo.hat <- lnLo(counts1 = samp1, counts2=samp2, counts3=samp3, Ntrials=N)
lnL1.hat <- lnL1(counts1 = samp1, counts2=samp2, counts3=samp3, Ntrials=N)

Gsq.obs <- -2*(lnLo.hat-lnL1.hat)
alpha <- 0.001
dfs <- 3-1 # 3 parameters estimated under H1 - 1 parameter estimated under Ho
Gsq.crit <- qchisq(p=1-alpha, df=dfs)
pvalue <- 1-pchisq(q=Gsq.obs, df=dfs)


Chisq.samp <- rchisq(n=100000, df=dfs)

hist(Chisq.samp)
abline(v=Gsq.obs, col="blue")
abline(v=Gsq.crit, col="red")


# PBLRT
B <- 2000 # Run a simulation of this data 2000 times (Bootstrap it!)
allsamps <- c(samp1,samp2,samp3)
phat.Ho <- sum(allsamps)/(N*length(allsamps)); # MLE under H0 = sum(xis)/sum(nis)
Gsq.vec <- rep(0,B) # Empty gsq vector
for(i in 1:B){
	# Simulate data like the one observed but under the Null hypothesis

	boot.data1 <- rbinom(n=n1, size=N, prob=phat.Ho) # Binomial sample of size N1, prob = to phat under the null, pretend that the Null Hypothesis is real 
	boot.data2 <- rbinom(n=n2, size=N, prob=phat.Ho) # "" for second experimental condition
	boot.data3 <- rbinom(n=n3, size=N, prob=phat.Ho) # "" for third experimental condition 
	
	lnLo.boot <- lnLo(counts1 = boot.data1, counts2=boot.data2, counts3=boot.data3, Ntrials=N)	# Compute the likelihood for this data
	lnL1.boot <- lnL1(counts1 = boot.data1, counts2=boot.data2, counts3=boot.data3, Ntrials=N)		
	
	Gsq.vec[i] <- -2*(lnLo.boot-lnL1.boot) # Compute the gsq per particular case
	
}

boot.pval <- sum(Gsq.vec > Gsq.obs)/2000 # Proportion of boostrap Gsq's that are bigger than the observed Gsq
print(boot.pval) # compare to Chisquare pvalue
print(pvalue)

par(mfrow=c(1,2))
hist(Gsq.vec, main = "Pboot Gsq dist.", xlim = c(0,15))
abline(v = Gsq.obs, col = "blue")
hist(Chisq.samp, main = "Wilk's Gsq dist", xlim = c(0,15))
abline(v=Gsq.obs, col="blue")
abline(v=Gsq.crit, col="red")



