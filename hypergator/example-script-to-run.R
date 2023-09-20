### A simple script to showcase how to call an Rscript using a shell SLURM Request. 

# Set up your directory calls from the shell script
setwd("/blue/millerjared/useful-code-depository/hypergator")
data_dir <- args[1]
plot_dir <- args[2]

# data_dir <- "/blue/millerjared/useful-code-depository/hypergator/data/" # for reference purposes
# plot_dir <- "/blue/millerjared/useful-code-depository/hypergator/plots/" # for reference purposes

# Simulation experiment: Law of Large numbers: Increasing the sample size should get us closer to the theoretical sample mean of a population of samples

# Set our parameters
mew <- 100 # mean
variance <- 30 # sigma^2
sd <- sqrt(30) # sd is the sqrt of the variance 
n_i <- c(5,10, 15, 20, 25, 50,100, 500, 1000, 10000) # sample sizes



holding_v <- rep(NA, 10000) # create a null holding string
holding_list <- list() # create a holding list
for(i in 1:length(n_i)){ # outer loop, to go through the possible sample sizes of n_i
  holding_v <- vector("list", 10000)
  for(j in 1:10000){ # inner loop, to conduct the 10000 random samples for each n_i
    random_sample <- rnorm(n_i[i], mean = mew, sd = sd)# use our pdf_function
    holding_v[[j]] <- random_sample # store this within our holding vector
  }
  holding_list[[i]] <- holding_v # store this within our holding list 
}

print("finished sampling normal distribution and building holding list")

mean_lists <- list()
for(j in 1:length(n_i)){
  mean_storage <- rep(NA, 10000)
  for(i in 1:10000){
    extracted_samples <- holding_list[[j]][i]
    extracted_samples <- unlist(extracted_samples)
    mean_of_samples <- mean(extracted_samples)
    mean_storage[i] <- mean_of_samples
  } # end of inner loop
  mean_lists[[j]] <- mean_storage
} # end of outer loop 

print("finished constructing mean list")

# Extract these data and store
for(i in 1:length(mean_lists)){
write.csv(mean_lists[[i]], paste0(data_dir, "/mean-extraction-for-", n_i[[i]], ".csv"))
}

print("finished writing mean datasets")
# now Plot the histogram of the sampled means, for each of the n_i's 
for(i in 1:length(mean_lists)){
  png(filename=paste0(plot_dir,"/plot-with-", n_i[[i]], "-samples", ".png"), width = 600, height = 600)
  hist(mean_lists[[i]], main = paste0("Histogram of n_i = ", n_i[[i]]))

  dev.off()
  
}
print("finished plotting")

