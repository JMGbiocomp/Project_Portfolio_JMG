import argparse

## --- Program Argument Managment --- ##
parser = argparse.ArgumentParser(
    prog=' fastaProcessor',
    description= 'parses through a fasta file to convert multi-line sequences into a single line and formats the file to be a seqeunce header followed by a one line sequence.',
    epilog='fasta file reformated in this manner are easier to handle in pipelines'
)

parser.add_argument('path_to_fasta_file')
parser.add_argument('path_to_new_file')


args = parser.parse_args()
fasta_file = args.path_to_fasta_file
new_file = args.path_to_new_file

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

## --- Program Script --- ##

fasta_processor(fasta_file, new_file)