#!/bin/bash

chmod a+x exomes
#change permissions of the exomes directory.
motif_list=$(cat motif_list.txt)
code_name_list=$(cat exomesCohort.txt)
#Creates vectors from the list of potential motifs and the identified exomes from samples with a diameter of 20-30 mm
mkdir topmotifs_joshua
chmod a+rwx topmotifs_joshua
#Creates a directory to store all the genes identified to have at least one of the top 3 motifs per exome file

for exome in $code_name_list; do
chmod a+rx exomes/$exome.fasta
#change permissions of the .fasta file for each identified exome to extract data from.  
    for motif in $motif_list; do
        touch counted_motifs_$exome.txt 
        motif_count=$(grep -o $motif exomes/$exome.fasta | wc -l)
        echo $motif_count $motif >> counted_motifs_$exome.txt
        #Creates a document with counts for the occurances of each potentially present motif
    done
    m1=$(cat counted_motifs_$exome.txt | sort -nr | head -n 3 | awk 'NR == 1 {print $2}')
    m2=$(cat counted_motifs_$exome.txt | sort -nr | head -n 3 | awk 'NR == 2 {print $2}')
    m3=$(cat counted_motifs_$exome.txt | sort -nr | head -n 3 | awk 'NR == 3 {print $2}')
    #sorts the counts for each motif from the motif_list.txt file to assign the top 3 highest occuring motifs to variables.
    ##Note: separate commands were used for each variable to avoid an issue of not recognizing each motif as its own string with a vector.
    grep -EB1 "$m1|$m2|$m3" exomes/$exome.fasta > topmotifs_joshua/$exome'_topmotifs'.fasta
    #searches the correspoinding exome.fasta file for any sequence with either of the top 3 motifs and places them along with their headers into a new .fasta file.
    rm counted_motifs_$exome.txt
    #removes the intermediate file, counted_motifs_$exome.txt, created by the script to keep the directory oorganized and uncluttered.
done