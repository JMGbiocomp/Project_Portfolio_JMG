## === RNAseqSim === ###
# Function to generate a synthetic RNAseq count data set intended for pipeline development and hypothesis testing
# Arguments:
# seed = integer value for a seed number to make the data set reproducible 
# species = the number of species (genes) in the data set (integer value of 1 or more); default set to 1000
# samples = the number of samples per treatment (integer value of 1 or more); default set to 10
# treatments = the number (integer value of 1 or more) of treatments in the data set for DEG analysis; default set to 2
# replicates = the replicate count per sample (integer value 1 or more); default set to 1
# Mrange = integer vector of length three for the range of values for means of the distributions used for sampling; min mean, max mean, and increment of sequence generation; default set to (20,10000,1)
# DEG = numeric value between 0 and 1 to determine the percentage of genes intended to be differentially expressed between treatment groups; default set to 0.01
# low.variance = numeric value between 0 and 1 (percentage) to determine the percentage of genes that will exhibit low variance; default set to 0.7
# z.factor = numeric value between 0 and 1 to determine the percentage of low variance genes that can display poor counts typical of RNAseq data sets; default set to 0.5
# normalize = TRUE or FALSE to indicate if the generated count data set is normalized between samples; ; default set to TRUE
# Function prints the indexes of the DEGs within the data set and outputs the data set as a data frame

source("function_PackageDependency.R")
package_list = c("stats", "randomNames")
VerifyRequiredPackages(package_list)
library("stats"); library("randomNames")

RNAseqSim = function (seed = NA, species = 1000, samples = 10, treatments = 2, treatment.names = NA, replicates = 1, Mrange = c(20,10000, 1), DEG = 0.01, low.variance = 0.7, z = 0.5, normalize = TRUE) {
  # flow control for controlling reproducibility of the data set
  if (!is.na(seed)) { set.seed(seed) }
  
  # Indexing for DEGs and low variance genes 
  species.index = 1:species # vector of all species indexes
  DEG.ct = round(species*DEG) #total number of DEGs in data set
  DEG.index = sample(x = species.index, size = DEG.ct) # random indexes for DEGs in data set
  print("DEG indexs:")
  print(DEG.index)
  
  species.index = setdiff(species.index, DEG.index)
  lvar.ct = round(species*low.variance)
  lvar.index = sample(x = species.index, size = lvar.ct)
  z.factor = seq(0.01, z, 0.01)
  zeros.percent = sample(x = z.factor, size = 1)
  zero.ct = round(lvar.ct*zeros.percent)
  zero.index = sample(x = lvar.index, size = zero.ct)
  lvar.index = setdiff(lvar.index, zero.index)
  
  
  # Sequences for sampling
  seq.mean = seq(Mrange[1], Mrange[2], Mrange[3]) # vector of all possible means (lamdas) for generating poisson distributions of high variance genes
  seq.lvar = seq(1, 100, 1) # vector of all possible means (lamdas) for generating poisson distributions of low variance genes
  seq.zero = c()
  seq.zero[1:100] = 0
  seq.zero = c(seq.zero, 1:100) # sequence to sample count values for low count genes typical in RNAseq data sets
  
  # Output data frame as the simulated RNAseq data set
  RNAseqData = data.frame(matrix(0, nrow = species, ncol = samples*replicates*treatments)) # output data frame
  sample.labels = c() # hold the treatment labels for samples
  # flow control to generate vectors of sample labels for all treatments
  for (t in 1:treatments) {
    if (!is.na(treatment.names)) {
      group.name = treatment.names[t]
      labels = c()
      labels[1:(samples*replicates)] = group.name
      sample.labels = c(sample.labels, labels)
    } else {
      group.name = randomNames(n = 1, which.names = "last") # simulates a treatment group as a "last name"
      labels = c()
      labels[1:(samples*replicates)] = group.name # vector length matches treatment sample count
      sample.labels = c(sample.labels, labels) # concatenate labels
    }
  }
  colnames(RNAseqData) = sample.labels # name columns with sample labels
  
  # top level flow control for generating RNAseq data for every gene in the simulation
  for (s in 1:species) {
    # flow control for checking random index of DEGs
    if (s %in% DEG.index) {
      data.ct = samples*replicates # number of samples including replicates per treatment
      species.data = c() # empty vector for sample values per species
      # flow control to sample counts from separate distributions to simulated DEGs
      for (t in 1:treatments) {
        treatment.mean = sample(x = seq.mean, size = 1) # randomly select a mean (lambda) for treatment
        sim.distribution = rpois(n = 10000, lambda = treatment.mean) # simulate a poisson distribution with a large set of values
        treatment.data = sample(x = sim.distribution, size = data.ct) # sample from poisson distribution to simulate a sample of RNA transcript counts  
        species.data = c(species.data, treatment.data) # concatenate simulated sample for species data
      }
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    } else if (s %in% lvar.index) {
      data.ct = samples*replicates*treatments # total number of samples including all replicates and across all treatments
      species.mean = sample(x = seq.lvar, size = 1) # randomly select a mean (lambda) for all treatments to represent low variance genes
      sim.distribution = rpois(n = 10000, lambda = species.mean) # simulate a poisson distribution with a large set of values
      species.data = sample(x = sim.distribution, size = data.ct) # sample from poisson distribution to simulate a sample of RNA transcript counts
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    } else if (s %in% zero.index) {
      data.ct = samples*replicates*treatments # total number of samples including all replicates and across all treatments
      species.data = sample(x = seq.zero, size = data.ct) # sample from sequence vector heavily weighted with zero values
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    } else {
      data.ct = samples*replicates*treatments # total number of samples including all replicates and across all treatments 
      species.mean = sample(x = seq.mean, size = 1) # randomly select a mean (lambda) for all treatments to represent high variance on-DEGs
      sim.distribution = rpois(n = 10000, lambda = species.mean) # simulate a poisson distribution with a large set of values
      species.data = sample(x = sim.distribution, size = data.ct) # sample from poisson distribution to simulate a sample of RNA transcript counts
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    }
  }
  
  # Flow control to handle "de-normalization" of the data set across all samples
  if (normalize == FALSE) {
    seq.normal = seq(0.70, 1, 0.001) # sequence of "skew" factors for "de-normalizing" the samples
    for (d in 1:(samples*replicates*treatments)) {
      skew.factor = sample(x = seq.normal, size = 1) # sample a single skew factor to adjust all count values for a specific sample replicate
      RNAseqData[,d] = round(skew.factor*RNAseqData[,d]) # adjusts the count values for current sample (column)
    }
  }
  return(RNAseqData) # returns the syntheitc RNAseq (counts) data set
}