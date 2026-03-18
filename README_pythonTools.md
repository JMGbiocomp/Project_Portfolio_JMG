# Python Tools README file

## Biological Sequence Analysis Tools

Description:
At the center of many bioinformatics and computational biology workflows are biolgical sequences (DNA, RNA, proteins, etc).  How these sequences compare to one another or to a reference sequence as well as what subsequences exist within a larger sequence are the backbone for many sequence and variant analysis pipelines.  Tools with various algorithms allow for the meriad of recognitzed variant types with biology and specific seqeucnes to be detected as well as presented in a meaninful and readable manner.  These tools are geared towards the handling, analyzing, visualizing and generating reports for biolgical sequeneces in this context.

Tools List:
- 'function_compareSEQ.py'
- 'function_compareSEQ2.py'
- 'function_mutationAnalysis.py'

### function_compareSEQ.py

Description: 
Basic biological seqeunce analyzer to identify sequence substitutions in query sequences against one reference sequence.  Recongizes sequence IDs by stadnard fasta header formatting.  No functionality to processing fasta files with sequences spread across multiple lines and does not recognize insertion or deletion variants between sequences.  Input files (reference and query) must be formated as sequence ID ('>sequence_ID') followed by the corresponding biological sequence on the subsequent line.  Output is a report with query sequence number with corresponding mismatch indexes and the sequence identity as well as the reference and query files recorded. 

Required Packages:
- argparse
- os

Requried Files:
- reference_sequence file (fasta, txt or similar format)
- query_sequence file (fasta, txt, or similar format)

Outputs:
- compareSequences_output.txt

Arguements:
1. <file_path_reference_file> (positional)
2. <file_path_query_file> (positional)
- '--verbose' or '-v' (optional flag for reporting mismatch indexes and sequenece identity in real time per query)

Example Execution:

python function_compareSEQ.py -v <file_path_reference_file> <file_path_query_file>


### function_compareSEQ2.py

Description:
Upgraded basic biological sequence analyzer to identify sequence subsitituions in query sequences agaisnt one or more reference sequences.  Accepts single and multi-line sequences in fasta formating and includes the generation of intermediate txt files produced from inputted fasta files.  No functionality for recognitzing insertions or deletions between query sequences and reference sequences.  Input files must include a sequence ID per seqeunce in standard fasta format followed by the corresponding sequence.  Final output is a report with the referecne and query seqeunce IDs per comparison, mismatch indexes, sequence identity per comparison, and the file paths for each input file.

Required Packages:
- argparse
- os

Requried Files:
- reference_sequence file (fasta, txt or similar format)
- query_sequence file (fasta, txt, or similar format) 

Outputs:
- 'reference.txt' (optional intermediate txt file of processed reference sequence fasta file)
- 'query.txt' (optional intermeidate txt file of processed query sequence fasta file)
- <report_file> (user named report file, ideally formated as txt, with sequence analysis results)

Arguements:
1. <file_path_reference_file> (positional)
2. <file_path_query_file> (positional)
3. <file_path_report_file> (positional)
- '--verbose' or '-v' (optional flag for reporting mismatch indexes and sequenece identity in real time per query)
- '--fasta' or '-f' (optinal flag for indicating that the input files are fasta formated and need to be processed to account for multi-line sequences)
- '--remove' or '-r' (optional flag to delete intermediate files processed form fasta file processing)

Example Execution:

python function_compareSEQ2.py -fr <file_path_reference_file> <file_path_query_file> <file_path_report_file>


### function_mutationAnalysis.py

Description: 
A sequence variant analysis tool to identfiy point mutations (DNA/RNA) and missense mutations (Proteins).  Implements a 'shrinking window' algorithm to identify single substituions, deletions, and insertions with accuracy and limited in its applicaiton for accurately detecting consecutive mutations (not intended for tanslocations or inversion mutations).  Functionality includes fasta file processing and defining the window size at the user-level to faciliate accurate mutation detection.  Input files must include a sequence ID per seqeunce in standard fasta format followed by the corresponding sequence. Final output is a report with reference and query sequence ID per comparison as well as the mutation type, reference sequence index and query sequence index in a stadnard tab delimited format.  The file paths for the reference and query files is included as a reference.   

Required PAckages:
- argparse
- os

Requried Files:
- reference_sequence file (fasta, txt or similar format)
- query_sequence file (fasta, txt, or similar format)

Outpouts:
- 'reference.txt' (optional intermediate txt file of processed reference sequence fasta file)
- 'query.txt' (optional intermeidate txt file of processed query sequence fasta file)
- <report_file> (user named report file, ideally formated as txt, with sequence analysis results)

Arguments:
1. <file_path_reference_file> (positional)
2. <file_path_query_file> (positional)
3. <file_path_report_file> (positional)
- 'window' or '-w' (nonpositional argument with a default value of '7' for the window size used with the analysis algorithm; does not need to be used or defined unless changing the window size AND must be a odd value integer)
- '--verbose' or '-v' (optional flag for reporting mismatch indexes and sequenece identity in real time per query)
- '--fasta' or '-f' (optinal flag for indicating that the input files are fasta formated and need to be processed to account for multi-line sequences)
- '--remove' or '-r' (optional flag to delete intermediate files processed form fasta file processing)

Example Execution:
python function_mutationAnalysis.py -fv <file_path_reference_file> <file_path_query_file> <file_path_report_file> -w 11 
