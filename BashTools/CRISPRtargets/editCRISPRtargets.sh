#!/bin/bash

code_name_list=$(cat exomesCohort.txt)
for exome in $code_name_list; do
    sed -E 's/(.{20}GG)/A\1/' precrispr_joshua/$exome'_precrispr'.fasta > $exome'_postcrispr'.fasta
    #sed -i '/--/d' $exome'_postcrispr'.fasta
done
#edits all identifed "NGG" sites to include an A base before the "NGG" sequence for all the genes in each *_precrispr.fasta file
#removes any line with '--' created between nonconsecutive genes being identified in the *_precrispr.fasta files