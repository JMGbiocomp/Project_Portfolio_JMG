# CRISPR Targets Workflow

Author: Joshua Glowalla
Email: joshuaglowalla@Brandeis.edu
Secondary email: joshuaglowalla@gmail.com
Date: 10/23/2025

## Goal

From a select set of specimen exomes, select genes containing the top selection of motifs for Cas protein binding and editing gene sequences in silico for CRISPR-Cas system.  

## 'targetExomes.sh'

Description:
Reads the 'clinical_data.txt file containing a list of sample information (i.e. discoverer, location, diameter(mm), environment, status, and code_name) to determine which organsisms had a diameter of 20-20 mm (and a sequenced exome) and generates a list ('exomesCohort.txt')of these organisms by their code name.

## 'identifyTopMotifGenes.sh'

Description:
Using the list of code names in 'exomeCohort.txt', it searches for and counts each motif from the 'motif_list.txt' file within the (code name).fasta files for the total number occurrances.  Each .fasta contains the exome of the organisms identified to have a diameter of 20-30 mm and has its exome sucessfully sequenced.  After determining the counts of each motif within each exome, the it researches the exome files and extracts the gene names and their sequences that have at least one of the three top motifs identified for the respective exome to a new .fasta file ('(code name)_topmotifs.fasta'). 

## 'identifyCRISPRsites.sh '

Description:
Identifies a suitable CRISPR site for every gene inside the '(code name)_topmotifs.fasta' files.  CRISPR sites are identified by a "NGG" seqence where "N" is 20 of any base.  This generates a new .fasta file ('(code name)_precrispr.fasta') per '*_topmotifs.fasta' file.  

## 'editCRISPRtargets.sh'

Description:
Using the '*_precrispr.fasta' files, the identified suitable CRISPR sites are edited to include an "A" before the "NGG" found in the sequence for every gene.  

## 'exomeReport.py'

Description:
Generates a report of the union of genes amongst the exome cohort of the organisms idneitifed to have a diameter 20-30 mm and whose exomes are sequenced.  Report includes:
-Discoverer of the organism, location found, diameter, and environment type
-the number of genes with identified and edited CRISPR sites shared by the cohort
-list of cohort genes with CRISPR sites

## Files

### Inputs

<clinical_file_name>.txt

<exomes_directory/path/name> directory
    <exome_name>.fasta



### Outputs

## Workflow Execution

1. Navigate to directory with input/script files and check permissions. 
ls -1

