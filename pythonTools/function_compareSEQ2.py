import argparse
import os

## --- Program Argument Managment --- ##
parser = argparse.ArgumentParser(
    prog='compareSEQ2',
    description='compares sequences to input reference seqeunce for sequence identity with only basic seqeunce comparison functionality (substitutions)',
    epilog='For more complex sequence analysis, use _________'
)

parser.add_argument('path_to_reference_fasta')
parser.add_argument('path_to_query_fasta')
parser.add_argument('path_to_report_txt')
parser.add_argument('-v', '--verbose', actiion = 'store_true', help = '')
parser.add_argument('-f', '--fasta', action = 'store_true', help = 'flag for recongizing that input files are in fasta format and need to be processed on top of the sequence analysis.  Functionality includes header recognition and single vs multi-line sequences.')
parser.add_argument('-r', '--remove', action = 'store_true', help ='flag to remove intermediate txt files generated after processing the inputted fasta files, references and query sequences.')

args = parser.parse_args()
reference_file = args.path_to_reference_fasta
query_file = args.path_to_query_fasta
report_output = args.path_to_report_txt

## --- Functions for script --- ##

# Function to convert fasta file format in order to steamline analysis 
def fasta_processor (input_file, output_file):
    '''
    Docstring for fasta_processer: converts a multi-sequence fasta files with headers and sequences on multiple lines to a txt file with each sequence on one line preceeded by a header liner
    
    :param input_file: fasta file with headers and sequences
    :param output_file: file path for output txt file with one sequence per line separated by headers with squences IDs
    '''
    with open(output_file, 'a') as output: # open output file
        with open(input_file, 'r') as file: # open fasta file
            sequence = '' # empty variable to hold sequence
            header = '' # empty variable to hold header with sequence ID
            # flow control to move line by line to combine multi-line sequences into a single variable and identify sequence IDs
            for line in file:
                if line.startswith('>'): # sequence header
                    # flow control to handle first sequence header vs subseqeunt sequence headers recognition 
                    if len(sequence) > 0: # subsequent headers 
                        output.write(sequence + '\n')
                        output.write(header + '\n')
                    else: # first header of the fasta file
                        output.write(header + '\n')
                else:
                    sequence = sequence + line # concatenate multi-line sequences

# Function to analyze a query seqeunce against a reference sequence for substitutions and reports indexes and sequence identity
def seqAnalysis (reference_sequence, query_sequence):
    '''
    Docstring for seqAnalysis: 
    
    :param reference_sequence: reference sequence for analyzing a query against
    :param query_sequence: query sequence to compare against a reference sequence
    '''
    sequence_length = len(reference_sequence) # number of index in reference sequence
    indexes = '' # variable to hold found indexes with substitutions
    identity = 0 # variable to hold sequence identity calculation
    mismatch_ct = 0 # varaible to hold the mismatch counts 
    # flow control to compare each index of the query sequence against the reference sequence
    for i in range(sequence_length - 1):
        # flow control to handle the mismatch
        if query_sequence[i] != reference_sequence[i]:
            indexes = indexes + '{i}, ' # add index to variable
            mismatch_ct = mismatch_ct + 1 # increase mismatch count
    identity = (sequence_length - mismatch_ct)/mismatch_ct # calculate identity

    return indexes, identity # return tuple for reporting

# Function to generate a report from analyzing the query sequences against one or more reference sequences
def generateReport (reference_file, query_file, report_file):
    '''
    Docstring for generateReport
    
    :param reference_file: processed reference sequence file with one line sequences preceeded by a sequence header ID
    :param query_file: processed query sequence file with one line sequences preceeded by a sequence header ID
    :param report_file: report file path recording the sequence anaysis results for every query and reference sequence combination
    '''
    with open(report_file, 'w') as report: # generate and open a report txt file
        with open(reference_file, 'r') as reference: # open the reference file for reading
            # flow control for move through the reference file line by line
            for r in reference:
                if r.startswith('>'): # recognize sequence header
                    report.write('Reference Sequence:' + '\n')
                    report.write(r + '\n' + '\n')
                    if args.verbose:
                        print(r)
                else: # assumes non-headers are sequences
                    reference_seq = r # extract reference sequence 
                    with open(query_file, 'r') as query: # open the query file for reading 
                        # flow control for move through the query file line by line
                        for q in query:
                            if q.startswith('>'): # recognize sequence header
                                report.write(q + '\n')
                                if args.verbose:
                                    print(q)
                            else: # assumes non-headers are sequences
                                query_seq = q
                                analysis = seqAnalysis(reference_seq, query_seq) # analyze the query against the reference sequence
                                report.wrtie('Substitution Idexes: ' + analysis[0] + '\n')
                                report.write('Sequence Identity: ' + analysis[1] + '\n')
                                if args.verbose:
                                    print(analysis[1])
                        report.write('--- end of query analysis for current reference sequence ---' + '\n' + '\n')

## --- Program Script --- ##

# flow control to determine if input files need to be processed as fasta files or are pre-processed
if args.fasta:
    fasta_processor(reference_file, 'reference.txt')
    fasta_processor(query_file, 'query.txt')
    generateReport('reference.txt', 'query.txt', report_output)
else:
    generateReport(reference_file, query_file, report_output)

# flow control to determine if user indicated that the intermediate files are to be removed
if args.remove:
    os.remove('reference.txt')
    os.remove('query.txt')
    