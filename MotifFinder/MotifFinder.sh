#!/bin/bash

touch motif_count.txt
chmod a+rx motif_count.txt 
#creates the file to store the counts for the number of occurances of each motif and modifies permissions.
mkdir -m a=rwx motifs
#creates a directory with "rwx" permissions to store the list of genes idenified for every motif
echo 'Number of Genes Found to Contain Each Identified Motif Associated with Radiation Resistance in R. bifella Colony' >> ./motif_count.txt #appends a title into the motif count file

target_motifs="$1"
fasta_file="$2"

motif_list=$(cat $target_motifs)
echo $motif_list 
#turns the file with the target motifs into an array/list.  It can be used as a "list" for the written loop.
echo 'Target motifs: '$motif_list #displays the list of identified motifs.
echo 'Number of genes found for each motif:' #displays text as a pretext to the prints of the motif counts in the subsequent loop.

for motif in $motif_list; do
	echo $motif' '$(grep -c $motif $fasta_file) #prints the motif and count
	echo $motif $(grep -c $motif $fasta_file) >> ./motif_count.txt #appends file with motif count 
	touch ./motifs/$motif.fasta #generates a .fasta file for current motif in list
	grep -B1 $motif $fasta_file >> ./motifs/$motif.fasta #appends .fasta file with list of identified genes that contain the motif in question
done





