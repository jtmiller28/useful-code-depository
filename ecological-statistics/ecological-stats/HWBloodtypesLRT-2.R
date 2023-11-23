# J.M. Ponciano, 10-13-2015
# R program to calculate ML estimates for the blood type proportions
# a, b and o in the multinomial model derived from the H-W proportions of  
# human blood types
# Null Model:  Y1,Y2,Y3,Y4 ~ multinomial(n,p1,p2,p3,p4) #p1,p2,p3,p4 are the proportions of the blood types, under hardy weinberg its a + b + o
# p1 = a*a + 2*a*o
# p2 = b*b + 2*b*o
# p3 = 2*a*b
# p4 = o*o


# Function to compute the NEGATIVE log-likelihood under the Null hypothesis: the Hardy-Weinberg model
# (note that here, the skeptic/researcher's roles are reversed: the hypothesis of interest 
# in a goodness of fit test is usually the null hypothesis -so we would be happy if we fail to reject Ho-)
# We will minimize this function using a numerical algorithm:  The nelder-mead simplex.
negloglike.Ho <- function(guess, ntot, obs.counts){
	
	a <- 1/(1+ exp(-guess[1])) # constrains a between 0 and 1 #inverse logit fxn
	b <- 1/(1+ exp(-guess[2])) # constrains b between 0 and 1
	o <- 1-a-b # a + b + o = 1, has issues on particular guesses so see below
	arefreqspos <- sum(c(a,b,o) > 0) # Ask are the frequencies Positive
	if(arefreqspos != 3){return(.Machine$double.xmax)}else{ # Return Largest number in R , dont get stuck on the negatives by feeding it a ridiclously large number
		
		p1 <- a*a + 2*a*o
		p2 <- b*b + 2*b*o
		p3 <- 2*a*b
		p4 <- o*o
		pvec <- c(p1,p2,p3,p4)
		
		negloglike <- -dmultinom(x=obs.counts, size=ntot, prob=pvec, log=TRUE)
		
		return(negloglike)
		
		}
	
	}

# THIS IS THE LIKELIHOOD THAT CORRESPONDS TO THE FULLY PARAMETERIZED MULTINOMIAL MODEL 
# THAT EXPLAINS THE DATA PERFECTLY WELL BECAUSE THE ESTIMATED EXPECTED COUNTS ARE 
# EQUAL TO THE OBSERVED COUNTS:
# ESTIMATED PROBS  phat_i = yi/ntot
# ESTIMATED EXPECTED = ntot*phat_i = ntot*(yi/ntot) = yi
max.loglike.H1 <- function(ntot,obs.counts){
	# returns the maximized log-likelihood function under the alternative model	
	len       <- length(obs.counts)
	phats     <- obs.counts/ntot 
	return(dmultinom(x=obs.counts, size=ntot,prob=phats, log=TRUE))	
}


# Data from Rao, C.K. 1973. Linear Satistical Inference and its applications
y1 <- 182
y2 <- 60
y3 <- 17
y4 <- 176
n  <- 435

obs.counts <- c(y1,y2,y3,y4)
# Start with initial guesses 1/3, 1/3 for a and b (hence 1/3 for o as well)
# Even if we start at even frequencies, the Nelder-Mead algorithm should converge
# One can start the search anywhere. If the problem is too complicated, the likelihood surface
# can have multiple peaks and the Nelder-Mead can get stuck in a sub-obtimal peak. That's why
# re-starting the Nelder-Mead on the final values if often useful.  But for this problem, a simple
# search is enough.
guess1 <- c(log(0.33) - log(1-0.33), log(0.33) - log(1-0.33)) # Reverse transform (1/(1+e^-theta)) solve for theta, log-odds
mlest  <- optim(par=guess1, fn= negloglike.Ho, method="Nelder-Mead", ntot=n,obs.counts=obs.counts)
ab.mles<- 1/(1+exp(-mlest$par))
a.mle <- ab.mles[1]
b.mle <- ab.mles[2]
o.mle  <- 1-sum(ab.mles)
lnL0.hat <- (-1)*mlest$value
lnL1.hat <- max.loglike.H1(ntot=n,obs.counts = obs.counts)  


Gsq <- (-2)*(lnL0.hat - lnL1.hat)
Like.ratio <- exp(-0.5*Gsq) # Compute the likelihood ratio, a number between 0 and 1

npar.H1 <- 4-1 #(p1,p2,p3,p4)
npar.H0 <- 3-1 #(a,b,o)

df  <-  npar.H1 - npar.H0
alpha <- 0.05
Gsq.crit <- qchisq(p=1-alpha,df=df)
if(Gsq>Gsq.crit){print("Reject Ho")}else{print("Fail to reject Ho")}
pvalue <- 1-pchisq(Gsq,df)
print(pvalue)

# Expected number of counts:
p1.hat <- a.mle*a.mle + 2*a.mle*o.mle
p2.hat <- b.mle*b.mle + 2*b.mle*o.mle
p3.hat <- 2*a.mle*b.mle
p4.hat <- o.mle*o.mle
phat.vec <- c(p1.hat,p2.hat,p3.hat,p4.hat)
sum(phat.vec)
Expected.vec <- n*phat.vec


# Hand calculation of Gsq:
# if we had 0 counts, the log(0) would give us problem
# to protect against that we just make 0 the terms that have a log(0)
obs.counts[obs.counts==0] <- 1
Gsq.hand <- 2*sum(obs.counts*log(obs.counts/Expected.vec))
Gsq.hand
Gsq

# Printing a table of expected vs. observed
blood.types <- c("A", "B", "AB", "O")
Exp.vs.Obs <- cbind(obs.counts,Expected.vec)
row.names(Exp.vs.Obs) <- blood.types
print(Exp.vs.Obs)


 
