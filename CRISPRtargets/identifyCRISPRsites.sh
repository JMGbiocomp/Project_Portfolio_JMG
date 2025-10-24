#!/bin/bash

mkdir precrispr_joshua
chmod a+rwx precrispr_joshua
#creates an accessible directory for all the precrispr.fasta files


code_name_list=$(cat exomesCohort.txt)
for exome in $code_name_list; do
    grep -EB1 '.{20,}GG' topmotifs_joshua/$exome'_topmotifs'.fasta > prescrispr_joshua/$exome'_precrispr'.fasta
done
#uses the exome list to search each cohort exome for potential CRISPR sites, "NGG", in the gene list and outputs all gene sequences including their headers to another file