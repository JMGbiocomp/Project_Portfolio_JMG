#!/bin/bash

# FIX mot generalized but functional code
chmod a+rx clinical_data.txt
awk 'BEGIN{FS="\t"} {if ($3 >= 20 && $3 <= 30 && $5 == "Sequenced") {print $6}}' clinical_data.txt > exomesCohort.txt
#identifies eukaryotes with diameters of 20-30 mm and whose exomes are already sequenced within the "clinical_data.txt" file and puts them into a new file, "exomesCohort.txt".

# FIX Generalized code

clinical_data = "$1"
chmod a+rx $clinical_data

search="$2"
search_ct="$#"
pattern_1="$3"
field_1="$4"
pattern_2="$5"
field_2="$6"
pattern_3="$7"
field_3="$8"
pattern_4="$9"
field_4="$10"

pattern_ct=$search_ct - 1
pattern_array=()
for i in {3..$pattern_ct..2}; do
    FIELD_NUM=$i
    pattern_array+=$FIELD_NUM
done

if [search == 'String']; then
    for i in {4..$search_ct..2}; do
        FIELD_NUM=$i
        pattern_index=$FIELD_NUM - 4
        SEARCH_VALUE=$pattern_array[$pattern_index]
        awk -v Field_to_check="$FIELD_NUM" -v value_to_find="$SEARCH_VALUE" 'BEGIN{FS="\t"} {if ($FIELD_NUM ==  $SEARCH_VALUE) {print $6}}' $clinical_data > exomesCohort.txt
    done
elif [search == 'MultiString']; then

elif [search == 'SingleNumber']; then

elif [search == 'MultiNumber']; then

else


