#!/bin/bash
#SBATCH --job-name=rserver
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=8gb
#SBATCH --time=48:00:00
#SBATCH --output=rserver_%j.log
module purge; module load R
rserver