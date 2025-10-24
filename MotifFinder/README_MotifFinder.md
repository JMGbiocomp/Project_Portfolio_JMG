# Motif Finder

Author: Joshua Glowalla
Email: joshuaglowalla@Brandeis.edu
Secondary email: joshuaglowalla@gmail.com
Date: 10/23/2025

## Goal
Execute a motif analysis for a set of identified motifs on a r_bifella.fasta, which contains sequencing data for a R. bifella colony.

## Description ##
This script executes a motif analysis for a set of identified motifs on r_bifella.fasta.  It will search for the motifs within the list of genes and count the number of genes found for each motif. Any gene sequence found containing a given moitif will be extracted along with the gene header into a seperate FASTA file contained within the generated "motifs" directory.

## Files 

Script:

'MotifFinder.sh'
Script to count motif abundance and extract genes with one or more of the targeted motifs.

Required Input:

'{file_name}.fasta'
FASTA file containing DNA sequences with a header of each gene names followed by the gene sequence on the subsequent line.

{file_name}.text''
A text file containing the list of identified motifs (one motif per line). 


Output:

A file named "motif_count.txt" listing the counts for each provided motif in the FASTA file.
For every inputted motif, a seperate FASTA file will be generated with the gene headers and corresponding gene sequences where the motif appears. These file can be found within the newly generated "motifs" directory.

# How to RUN:

Navigate to the directory containing the script.
Run the following commands:

chmod a+rx week2script.sh
bash week2script.sh <file/path/target_motifs.txt> <file/pathe/gene_list.fasta>