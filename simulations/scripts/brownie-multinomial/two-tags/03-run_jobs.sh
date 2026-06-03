#!/bin/bash
#SBATCH --account=def-emartins
#SBATCH --time=02-00
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=8000M
#SBATCH --mail-user=ehirsch@unbc.ca
#SBATCH --mail-type=ALL
#SBATCH --array=1-729

echo "Running on host `hostname`"

echo "Working directory is `pwd`"

module load StdEnv/2020

module load gcc/9.3.0

module load r/4.3.1

module load jags/4.3.2

echo "Starting R at `date`."

srun $(head -n $SLURM_ARRAY_TASK_ID jobs03.txt | tail -n 1)

echo "Finished R at `date`."