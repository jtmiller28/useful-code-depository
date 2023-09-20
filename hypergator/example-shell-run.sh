#!/bin/bash

#SBATCH --job-name=example-plotting      # Job name
#SBATCH --mail-type=ALL                  # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=jtmiller@ucsb.edu    # Where to send mail
#SBATCH --output=%j.log                  # Standard output and error log
#SBATCH --nodes=1                        # Run all processes on a single node
#SBATCH --ntasks=1                       # Run a single task
#SBATCH --cpus-per-task=1                # Number of CPU cores per task
#SBATCH --mem=3gb                        # Job memory request
#SBATCH --time=00-01:00:00               # Time limit days-hrs:min:sec
#SBATCH --qos=soltis-b
pwd; hostname; date

#load modules

module load R/4.2

#do some (or alot) of coding
Rscript --vanilla /blue/soltis/millerjared/pollen-project/Pollen-DB/pollen-project-cluster/plant-parallel-script-SiTsTm.R $1 $2 

##example for running: sbatch /blue/soltis/millerjared/useful-code-depository/hypergator/example-shell-run.sh /blue/soltis/millerjared/hypergator/data/ /blue/soltis/millerjared/hypergator/plots/
