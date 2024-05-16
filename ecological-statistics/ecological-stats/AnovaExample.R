#ANOVA sums of squares calculator:
ssqs.fun <- function(my.data){

	ni  <- dim(my.data)[1] # ni is the sample sizes (under the assumption of same sample sizes)
	g   <- dim(my.data)[2] # number of groups
	nT  <- ni*g # totla sample size
	allxs    <- as.vector(my.data)
	xbar     <- mean(allxs) # overall mean
	xbari.vec<- apply(my.data,2,FUN=mean) # apply means 
	TSS <- (nT - 1)*var(allxs) # variance *nT-1 (estimate)
	SSB <- sum( ni*(xbari.vec-rep(xbar,g))^2 ) # sum of sqs between
	SSW <- TSS-SSB # sum of sqs within
	
	Stot.sq <- TSS/(nT-1) # total variance est
	Sw.sq <- SSW/(nT-g) #  variance within
	Sb.sq <- SSB/(g-1) # variance between
	Fval <- Sb.sq/Sw.sq # test statistic, to show the variance between/variance within (star shaped diagram)
	pval <- 1-pf(q=Fval,df1=(g-1),df2 = (nT-g)) # pvalue to show how extreme these obs is
	
	
	return(list(TSS=TSS, SSB=SSB,SSW=SSW, Stot.sq=Stot.sq,Sw.sq=Sw.sq,Sb.sq=Sb.sq,Fval=Fval,pval=pval,df1=(g-1), df2 = (nT-g)))
}


# A horticolturist was investigating the phosphorus content of tree leaves from three different varieties of apple trees (1,2 
# and 3).  Random samples of five leaves from each of the three varieties were analyzed for phosphorus content.  The data are
# given below.  Are the mean levels of phosphorus contents equal across the three varieties?

Ph.cont <- c(0.35,0.40,0.58,0.50, 0.47, 0.65,0.70,0.90, 0.84,0.79,0.6,0.80,0.75,0.73,0.66)
sp1 <- Ph.cont[1:5]
sp2 <- Ph.cont[6:10]
sp3 <- Ph.cont[11:15]
samp.sizes <- c(5,5,5)

Ph.data <- cbind(sp1,sp2,sp3)

### Looking at the F distribution:

rFs <- rf(n=10000, df1=3-1, df2=15-3)
hist(rFs)
abline(v=mean(rFs))

Ph.SSQs<- ssqs.fun(Ph.data) # throw the data to the function

VarB.M.VW <- Ph.SSQs$Sb.sq - Ph.SSQs$Sw.sq # The variance between - the variance within (the differences between the variances)

means <- apply(Ph.data,2,mean)
samp.vars  <- apply(Ph.data,2,var) # sample variances


# Creating a data frame to do the ANOVA using R's canned function 'aov'
Species <- as.factor(rep(c("Sp1", "Sp2", "Sp3"), each=5))
species <- as.factor(rep(c(1,2,3), each=5)) # You HAVE to make it a classification variable!!
Ph.dataframe <- data.frame(species,Ph.cont)

my1stAOV <- aov(Ph.cont~species,data=Ph.dataframe) # Simply say that the phosphorus content is a function of the species 
summary(my1stAOV) # Compare this to our Ph.SSQs output

library(MASS)
my1stAOV.res <- my1stAOV$residuals # deviations from the group means
my1stAOV.stdres <- stdres(my1stAOV) # Residuals are great diagnostic tools (because they are a reflection whether the normality within each group is a good assumption, they should be about the same absolute size around the mean, if observations are very extreme it could be (not always) that the distribution isnt quite normal but rather fat tailed. These extreme observations can ruin an analysis)
# USes of residuals 
# 1. detect normality
# 2. detect outliers in the observations <- task of the analyst to figure out whats going on with that particular sample. 
# The standardized cutoff is a cut off for observations TOO extreme to come from the distribution
my1stAOV.studres <- studres(my1stAOV) #Studentized residuals abs(residuals) > 2 indicates an outlier
cbind(my1stAOV.res, my1stAOV.studres) # transformation into a scale that says if the residuals are above an abs size of 2, remove
# We can create an object that contains the summary of the aov function:
sum.aov <- summary(my1stAOV)
sum.aov

# The summary is a list composed of various objects.  One of them is the actual F table: 
table.aov <- sum.aov[[1]] # The summary is a list!
dim(table.aov)


# For example, if I want to retrieve the F value to use it later, I type:
Fval <- table.aov[1,4] # access the fvalue
Fval

# Now, if I want the estimate of sigma squared under the alternative, I can go and fetch the SSW
ssw.aov <- table.aov[2,2] 
ssw.aov
Ph.SSQs$SSW #just to see if we got it right

# and then divide the SSW by the degrees of freedom to get my estimate sigmasq.hat under the alternative:
ssw.df <- table.aov[2,1]
# sigmasq.hat is calculated with
ssw.aov/ssw.df

# or alternatively, get it from the F table directly!
sigsq.hat <- table.aov[2,3]

# Fetching the pvalue:
my.pval <- table.aov[1,5]



###  Predicted vs. residuals: (How do the means change as a function of the residuals) (Diagnostic Tool)
# Why is this important? Mean and Variance are independent in Normal Distribution: If you plot these, the dispersion should be identical while the means remain the same. If the dispersion changes with the means, we have a different distribution (poisson etc)
pred.vals <- predict(my1stAOV) # retrieves the predicted mean in each category of the model you are proposing. 

par(mfrow=c(1,2), oma=c(1,1,1,1), mar=c(5,5,5,3))
plot(pred.vals,my1stAOV.res, xlab="Predicted values", ylab="Residuals", pch=16, main= "Predicted vs. Residuals", cex.lab=1.5, cex.axis=1.25, cex.main=1.5)
abline(h=0, lty=2) # add a horizontal line of type 2 ("lty" stands for line type)
plot(pred.vals,my1stAOV.studres, xlab="Predicted values", ylab="Studentized Residuals", pch=16, main= "Predicted vs. Residuals", cex.lab=1.5, cex.axis=1.25, cex.main=1.5)
abline(h=0, lty=2) 

par(mfrow=c(2,2))
plot(my1stAOV)


# How do we work with any other probability distribution???
# Go into the formulas
my1stAOV <- aov(Ph.cont~species, data = Ph.dataframe) # Look into design matrix
# Look a wikipedia search on summing, multiplying, and inversing matrices
