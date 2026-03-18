### === RNAseqSim2 === ###
# Function to generate a synthetic RNAseq count data set intended for pipeline development and hypothesis testing.
# Functionality includes control over experimental design aspects (species, samples, replicates, and treatments) as well as data point characteristics to resemble experimental treatment effects.  
# Arguments allow for control over the differentiated and low variacne species within the data set as well as how poor counts are handled, which includes replacement functionality.  
# ------------------------------------------------------------------------------------------------------------ #
# Arguments:
# species = the number of species (genes) in the data set (integer value of 1 or more); default set to 1000
# samples = the number of samples per treatment (integer value of 1 or more); default set to 10
# replicates = the replicate count per sample (integer value 1 or more); default set to 1
# treatments = the number (integer value of 1 or more) of treatments in the data set for DEG analysis; default set to 2
# treatment.names = names of the experimental groups (treatments) to be used for generating the data set; default is set to NA to randomly generate treatment names and by default the fist treatment is considered the control group
# mean.range = integer vector of length three for the range of values for means of the distributions used for sampling; min mean, max mean, and increment of sequence generation; default set to (20,10000,1)
# DEG.factor = the number of differentially expressed species (between 0 and 1) within the data set; default set to 0.1 (10%)
# DEG.up = vector of 3 elements for the range of fold change values for up regulated species by treatment; default is set to c(1.2, 4, 0.01) for sequence generation
# DEG.down = vector of 3 elements for the range of fold change values for down regulated species by treatment; default is set to c(0.01, 0.5, 0.01) for sequence generation
# DEG.treatments = vector of values either 0, 1, or 2 matching the value of the treatments argument; 0 indicates the control group, 1 indicates a treatment not differentially expressed, and 2 indicates a differentially expressed treatment from the control group
# NEG.factor = value of the incremental difference between the maximum fold change value of up/down regulated species and "no change"
# lvar.factor = the number of low variance species (between 0 and 1) within the data set; default set to 0.7 (70%)
# lvar.range = vector of 3 elements for the range of count values for generating low variance species; default is set to c(1, 400, 1) for sequence generation
# p.factor = the number species with samples with poor counts (between 0 and 1) 
# p.range = vector of 3 elements for determining sample counts for species designated as having poor counts overall; default is set to c((1,100,100)) where the first two elemetns define the range of count values and the third element defines the number of zero placed within the pool of values for sampling
# seed = integer value for a seed number to make the data set reproducible 
# norm.sample = logical value (TRUE or FALSE) for determining if total counts by samples will be (nearly) normalized; default is set to TRUE
# norm.range = vector of 3 elements for the range of values to skew the count values per sample; default set to c(0.7, 1.3, 0.001) for sequence generation
# data.replacement = logical value (TRUE or FALSE) for determining if counts will undergo replacement relative to a species' mean; default is set to FALSE
# replacement.range = vector of 3 elements for the range of percentages of the total species that will undergo replacement; default set to c(0.01, 0.2, 0.001) for sequence generation
# replacement.factor = vector of 3 elements for the range of factors to multiple to species mean to determine the replacement value: default set to c(0.5, 1.5, 1) for sequence generation



source("function_PackageDependency.R")
package_list = c("stats", "randomNames")
VerifyRequiredPackages(package_list)
library("stats"); library("randomNames")

RNAseqSim2 = function (species = 1000, samples = 10, replicates = 1, treatments = 2, treatment.names = NA, mean.range = c(20, 10000, 1), DEG.factor = 0.1, DEG.up = c(1.2, 4, 0.01), DEG.down = c(0.01, 0.5, 0.01), DEG.treatments = c(0,2), NEG.factor = 0.01, lvar.factor = 0.7, lvar.range = c(1,400,1), p.factor = 0.5, p.range = c(1,100,100) , seed = NA, norm.sample = TRUE, norm.range = c(0.7, 1.3, 0.001), data.replacement = FALSE, replacement.range = c(0.01, 0.2, 0.001), replacement.factor = c(0.5, 1.5, 1)) {
  # argument flags
  if (length(DEG.treatments) != treatments) {stop("DEG.treatment argument length does not match the number of desired treatments for the synthetic data set.")}
  if (length(mean.range) > 3) {warning("Inputed vector for 'mean.range' argument assumed to be a complete sequence of values representing all possbile means for each treatment. To automate this feature, vector must be length of 3 values: (1) min mean value, (2) max mean value, and (3) increment value for generating a sequence.")}
  if (norm.sample == FALSE) {warning("Count data is not normalizxed by sample.  Recommended that normalization methods should be evalauted for best fit to the synthetic data set.")}
  if (data.replacement == TRUE) {warning("Data set generated with replacement.")}
  
  # flow control to set seed for reproducible data sets
  if (!is.na(seed)) { set.seed(seed) }
  
  # Data set characteristic indexing
  species.index = 1:species # vector of all species indexes
  DEG.ct = round(species*DEG.factor) # total number of DEGs in data set
  DEG.index = sample(x = species.index, size = DEG.ct) # random indexes for DEGs in data set
  print("DEG indexs:")
  print(DEG.index)
  
  species.index = setdiff(species.index, DEG.index)
  lvar.ct = round(species*lvar.factor) # total number of intentional low variance species 
  lvar.index = sample(x = species.index, size = lvar.ct)
  p.ct = round(lvar.ct*p.factor) # total number of intentional poor count species
  p.index = sample(x = lvar.index, size = p.ct) # species index with poor count data
  lvar.index = setdiff(lvar.index, p.index) # species index with low variance 
  
  # Mean and poor count sampling pools
  if (length(mean.range) > 3) {
    seq.mean = mean.range # user inputed vector of all possible means (lamdas) for generating poisson distributions of high variance genes
  } else {
    seq.mean = seq(mean.range[1], mean.range[2], mean.range[3]) # vector of all possible means (lamdas) for generating poisson distributions of high variance genes
  }
  seq.lvar = seq(lvar.range[1], lvar.range[2], lvar.range[3]) # vector of all possible means (lamdas) for generating poisson distributions of low variance genes
  seq.p = c()
  seq.p[1:p.range[3]] = 0
  seq.p = c(seq.p, p.range[1]:p.range[2]) # sequence to sample count values for low count genes typical in RNAseq data sets
  # fold change value pools
  UpReg.seq = seq(DEG.up[1], DEG.up[2], DEG.up[3])
  DownReg.seq = seq(DEG.down[1], DEG.down[2], DEG.down[3])
  NoChange.seq = seq((DEG.down[2] + NEG.factor), (DEG.up[1] - NEG.factor), mean(DEG.up[3], DEG.down[3]))
  
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
  
  # top level flow control for generating count data for data set
  for (s in 1:species) {
    if (s %in% DEG.index) {
      data.ct = samples*replicates # number of data points for each treatment
      species.data = c() # vector to hold all the count data for each species 
      base.mean = sample(x = seq.mean, size = 1) # randomly select a mean (lambda)
      
      # flow control for each treatment
      for (t in treatments) {
        if (DEG.treatments[t] == 2) {
          DEG.effect = sample(x = 0,1, size = 1)
          # flow control to select either an up or down regulated gene for current treatment 
          if (DEG.effect == 1) {
            treatment.mean = round((sample(x = UpReg.seq, size = 1))*base.mean) # up regulated fold change applied to base mean of species 
          } else {
            treatment.mean = round((sample(x = DownReg.seq, size = 1))*base.mean) # down regualted fold change applied to base mean of species 
          }
        } else if (DEG.treatments[t] == 1) {
          treatment.mean = round((sample(x = NoChange.seq, size = 1))*base.mean) # "Not differentially expressed" fold change applied to base mean of species for treatment comparison indicated to not be differentially expressed from control
        } else {
          treatment.mean = base.mean # indicated as control group
        }
        treatment.data = sample(x = rpois(n = 10000, lambda = treatment.mean), size = data.ct) # sample from poisson distribution to simulate a sample of RNA transcript counts  
        species.data = c(species.data, treatment.data) # concatenate simulated sample for species data
      }
      
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    } else if (s %in% lvar.index) {
      data.ct = samples*replicates*treatments # total number of samples including all replicates and across all treatments
      species.mean = sample(x = seq.lvar, size = 1) # randomly select a mean (lambda) for all treatments to represent low variance genes
      species.data = sample(x = rpois(n = 10000, lambda = species.mean), size = data.ct) # sample from poisson distribution to simulate a sample of RNA transcript counts
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    } else if (s %in% p.index) {
      data.ct = samples*replicates*treatments # total number of samples including all replicates and across all treatments
      species.data = sample(x = seq.p, size = data.ct) # sample from sequence vector heavily weighted with zero values
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    } else {
      data.ct = samples*replicates*treatments # total number of samples including all replicates and across all treatments 
      species.mean = sample(x = seq.mean, size = 1) # randomly select a mean (lambda) for all treatments to represent high variance on-DEGs
      species.data = sample(x = rpois(n = 10000, lambda = species.mean), size = data.ct) # sample from poisson distribution to simulate a sample of RNA transcript counts
      RNAseqData[s,] = species.data # add RNA counts for all treatments to output data frame
    }
  }
  # handling normalized by sample functionality
  if (norm.sample == FALSE) {
    seq.normal = seq(norm.range[1], norm.range[2], norm.range[3]) # sequence of "skew" factors for "de-normalizing" the samples
    for (d in 1:(samples*replicates*treatments)) {
      skew.factor = sample(x = seq.normal, size = 1) # sample a single skew factor to adjust all count values for a specific sample replicate
      RNAseqData[,d] = round(skew.factor*RNAseqData[,d]) # adjusts the count values for current sample (column)
    }
  }
  # handling replacement functionality 
  if (data.replacement == TRUE) {
    replacement.ct = round(samples*replicates*treatments*(sample(x = seq(replacement.range[1], replacement.range[2], replacement.range[3]), size = 1))) # number of data points to undergo replacement 
    r.species = c()
    r.sample = c()
    
    for (r in 1:replacement.ct) {
      replacement.species = sample(x = 1:species, size = 1) # selects a species (row)
      replacement.sample = sample(x = 1:samples*treatments*replicates, size = 1) # selects a sample (column)
      species.mean = mean(c(t(RNAseqData[replacement.species,]))) # determines the overall mean between all data points of a species
      replacement.value = round(sample(x = seq(round(replacement.factor[1]*species.mean), round(replacement.factor[2]*species.mean), replacement.factor[3]), size = 1)) # selects a replacement value based on the overall species mean and a given set of factos applied to the mean to generate a range of potential values
      RNAseqData[replacement.species, replacement.sample] = replacement.value # replacement of sample value
      r.species = c(r.species, replacement.species)
      r.sample = c(r.sample, replacement.sample)
    }
    print(paste("species index for replacement", r.species))
    print(paste("sample index for replacement", r.sample))
  }
  
  return(RNAseqData)
}