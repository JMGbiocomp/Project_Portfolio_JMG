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
parser.add_argument('-w', '--window', type = int, default = 4, help = 'defines the size of the window frame for analyzing the sequences to determine the mutation type: substitution, deletion or insertion; default value is 4')
parser.add_argument('-i', '-intermediate', type = str, default = '', help = '')

parser.add_argument('-v', '--verbose', actiion = 'store_true', help = '')
parser.add_argument('-f', '--fasta', action = 'store_true', help = 'flag for recongizing that input files are in fasta format and need to be processed on top of the sequence analysis.  Functionality includes header recognition and single vs multi-line sequences.')
parser.add_argument('-r', '--remove', action = 'store_true', help ='flag to remove intermediate txt files generated after processing the inputted fasta files, references and query sequences.')

args = parser.parse_args()
reference_file = args.path_to_reference_fasta
query_file = args.path_to_query_fasta
report_output = args.path_to_report_txt
window_size = args.window
intermediate_output = args.intermediate


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


def mutationIdentifier (reference_sequence, reference_index, query_sequence, query_index, window_size):
    '''
    '''
    mutation_found = False
    
    while mutation_found == False:
        window_indexes = window_framer(reference_sequence, reference_index, query_sequence, query_index, window_size)
        if query_sequence[window_indexes[2]:query_index] == reference_sequence [window_indexes[0]:reference_index] & query_sequence[query_index+1:window_indexes[3]] == reference_sequence[reference_index+1:window_indexes[1]]:
                mutation = 'substitution'
                report_reference = reference_sequence[reference_index]
                report_query = query_sequence[query_index]
                reference_index = reference_index + 1
                query_index = query_index + 1
                mutation_found = True
        elif query_sequence[window_framer[2]:query_index] == reference_sequence[window_indexes[0]:reference_index] & query_sequence[query_index:window_indexes[3]-1] == reference_sequence[reference_index+1:window_indexes[1]]:
                mutation = 'deletion'
                report_reference = reference_sequence[reference_index]
                report_query = 'NA'
                reference_index = reference_index + 1
                mutation_found = True
        elif query_sequence[window_framer[2]:query_index] == reference_sequence[window_indexes[0]:reference_index] & query_sequence[query_index+1:window_indexes[3]+1] == reference_sequence[reference_index:window_indexes[1]]:
                mutation = 'insertion'
                report_reference = 'NA'
                report_query = query_sequence[query_index]
                query_index = query_index + 1
                mutation_found = True
        else:
            window_size = window_size - 1
    return mutation, report_reference, report_query, reference_index, query_index


def SEQanalyzer (reference_sequence, query_sequence, report_file, window_size):
    '''
    '''
    if len(reference_sequence) == len(query_sequence):
        range_length = len(reference_sequence)
    elif len(reference_sequence) > len(query_sequence):
        range_length = len(reference_sequence)
    else:
        range_length = len(query_sequence)
    r_index = 0
    q_index = 0

    for i in range(range_length):
        if query_sequence[i] != reference_sequence[i]:
            analysis = mutationIdentifier(reference_sequence, r_index, query_sequence, q_index)
            report_file.write(analysis[0] + '\t' + analysis[1] + '\t' + analysis[2] + '\n')
            r_index = analysis[3]
            q_index = analysis[4]
        else:
            r_index = r_index + 1
            q_index = q_index + 1

def generateReport (reference_file, query_file, report_file, window_size):
    with open(report_file, 'a') as report:
        report.write('reference_sequence' + '\t' + 'query_sequence' + '\t' + 'mutation' + '\t' + 'ref_index' + '\t' + 'query_index' + '\n')
        with open(reference_file, 'r') as r_file:
            for r in r_file:
                if r.startswith('>'):
                    report.write(r + '\t')
                else:
                    reference_seq = r
                    with open(query_file, 'r') as q_file:
                        for q in q_file:
                            if q.startswith('>'):
                                report.write(q + '\t')
                            else:
                                query_seq = q
                                SEQanalyzer(reference_seq, query_seq, report, window_size)


## --- Program Script --- ##

if args.fasta:
    if args.intermediate:
        output_r = intermediate_output + '_reference.txt'
        output_q = intermediate_output + '_query.txt'
        fasta_processor(reference_file, output_r)
        fasta_processor(query_file, output_q)
        generateReport(output_r, output_q, report_output, window_size)

    else:
        fasta_processor(reference_file, 'reference.txt')
        fasta_processor(query_file, 'query.txt')
        generateReport('reference.txt', 'query.txt', report_output, window_size)
else:
    generateReport(reference_file, query_file, report_output, window_size)
