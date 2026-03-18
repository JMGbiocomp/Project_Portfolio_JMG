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
parser.add_argument('-w', '--window', type = int, default = 7, help = 'defines the size of the window frame for analyzing the sequences to determine the mutation type: substitution, deletion or insertion; default is a value of 7, 3 indexes before and 3 indexes after the target index.')

parser.add_argument('-v', '--verbose', actiion = 'store_true', help = '')
parser.add_argument('-f', '--fasta', action = 'store_true', help = 'flag for recongizing that input files are in fasta format and need to be processed on top of the sequence analysis.  Functionality includes header recognition and single vs multi-line sequences.')
parser.add_argument('-r', '--remove', action = 'store_true', help ='flag to remove intermediate txt files generated after processing the inputted fasta files, references and query sequences.')

args = parser.parse_args()
reference_file = args.path_to_reference_fasta
query_file = args.path_to_query_fasta
report_output = args.path_to_report_txt
window_size = args.window

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

def window_framer (reference_sequence, reference_index, query_sequence, query_index, window_frame):
    '''
    Docstring for window_framer
    
    :param reference_sequence: Description
    :param reference_index: Description
    :param query_sequence: Description
    :param query_index: Description
    :param window_frame: Description
    '''
    # flow control to handle the window frame for the query sequence
    if reference_index == 0:
        reference_upstream = reference_index
        reference_downstream = reference_index + window_frame
    elif (reference_index - window_frame) < 0:
        reference_upstream = 0
        reference_downstream = reference_index + window_frame
    else:
        reference_upstream = reference_index - window_frame
        if (reference_index + window_frame) > (len(reference_sequence) - 1):
            reference_downstream = len(reference_index) - 1
        else:
            reference_downstream = reference_index + window_frame
    # flow control
    if query_index == 0:
        query_upstream = query_index
        query_downstream = query_index + window_frame
    elif (query_index - window_frame) < 0:
        query_upstream = 0
        query_downstream = query_index + window_frame
    else:
        query_upstream = query_index - window_frame
        if (query_index + window_frame) > (len(query_sequence) - 1):
            query_downstream = len(query_sequence) - 1
        else:
            query_downstream = query_index + window_frame

    return reference_upstream, reference_downstream, query_upstream, query_downstream

    

def mutAnalysis (reference_sequence, query_sequence, window_size, report_file):
    '''
    Docstring for mutAnalysis
    
    :param reference_sequence: Description
    :param query_sequence: Description
    :param window_size: Description
    :param report_file: Description
    '''
    ref_length = len(reference_sequence)
    query_length = len(query_sequence)

    if ref_length > query_length:
        index_range = ref_length - 1
    elif ref_length < query_length:
        index_range = query_length - 1
    else:
        index_range = ref_length - 1

    reference_index = 0
    query_index = 0

    for i in range(index_range):
        if query_sequence[query_index] != reference_sequence[reference_index]:
            window_check = False
            while not window_check:
                window = (window_size - 1)/2
                window_indexes = window_framer(reference_sequence, reference_index, query_sequence, query_index, window)
                if query_sequence[window_indexes[2]:query_index] == reference_sequence [window_indexes[0]:reference_index] & query_sequence[query_index+1:window_indexes[3]] == reference_sequence[reference_index+1:window_indexes[1]]:
                    mutation = 'substitution'
                    report_reference = reference_index
                    report_query = query_index
                    with open(report_file,'a') as report:
                        report.write(mutation + '\t' + report_reference + '\t' + report_query + '\n')
                    reference_index = reference_index + 1
                    query_index = query_index + 1
                    window_check = True
                elif query_sequence[window_framer[2]:query_index] == reference_sequence[window_indexes[0]:reference_index] & query_sequence[query_index:window_indexes[3]-1] == reference_sequence[reference_index+1:window_indexes[1]]:
                    mutation = 'deletion'
                    report_reference = reference_index
                    report_query = query_index
                    with open(report_file,'a') as report:
                        report.write(mutation + '\t' + report_reference + '\t' + report_query + '\n')
                    reference_index = reference_index + 1
                    window_check = True
                elif query_sequence[window_framer[2]:query_index] == reference_sequence[window_indexes[0]:reference_index] & query_sequence[query_index+1:window_indexes[3]+1] == reference_sequence[reference_index:window_indexes[1]]:
                    mutation = 'insertion'
                    report_reference = reference_index
                    report_query = query_index
                    with open(report_file,'a') as report:
                        report.write(mutation + '\t' + report_reference + '\t' + report_query + '\n')
                    query_index = query_index + 1
                    window_check = True
                else:
                    window = window - 1
        else:
            pass
    pass

#
def generateReport (reference_file, query_file, report_file, window_size):
    '''
    Docstring for generateReport
    
    :param reference_file: Description
    :param query_file: Description
    :param report_file: Description
    :param window_size: Description
    '''
    with open(report_file, 'a') as report:
        report.write('Mutation Report Summary for Query Sequences \n')
        report.write('Reference Sequence file: ' + reference_file + '\n')
        report.write('Query Sequence file: ' + query_file + '\n')
        report.write('## --- Analysis Start --- ##' + '\n\n')

        with open(reference_file, 'r') as reference:

            for r in reference:

                if r.startswith('>'):
                    reference_ID = q
                    report.write('Reference Sequence ID:' + '\n')
                    report.write(reference_ID + '\n')
                else:
                    reference_seq = r
                
                with open(query_file, 'r') as query:

                    for q in query:

                        if q.startswith('>'):
                            query_ID = q
                            report.write('Query Sequence ID:' + '\n')
                            report.write(query_ID + '\n')
                            report.wtrie('Mutation' + '\t' + 'Reference' + '\t' + 'Query' + '\n')
                        else:
                            query_seq = q
                            mutAnalysis(reference_seq, query_seq, window_size, report_file)
                    report.write('# ---  All Query Sequences Evaluated --- #' + '\n\n')
            report.write('## --- Analysis End --- ##')


## --- Program Script --- ##

# flow control to determine if input files need to be processed as fasta files or are pre-processed
if args.fasta:
    fasta_processor(reference_file, 'reference.txt')
    fasta_processor(query_file, 'query.txt')
    generateReport('reference.txt', 'query.txt', window_size, report_output)
else:
    generateReport(reference_file, query_file, window_size, report_output)

# flow control to determine if user indicated that the intermediate files are to be removed
if args.remove:
    os.remove('reference.txt')
    os.remove('query.txt')