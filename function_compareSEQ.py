import argparse

# set up for program arguments 
parser = argparse.ArgumentParser(
    prog='compareSEQ',
    description='compares sequences to input reference seqeunce for sequence identity with onyl basic seqeunce comparison functionality (substitutions)',
    epilog='For more complex sequence analysis, use _________'
)

parser.add_argument('reference_file') # positional argument for file with reference sequence; first argument 
parser.add_argument('input_file') # positional argument for file with target sequences; second argument 
parser.add_argument('--verbose', '-v', action = 'store_true', help = 'Enables sequence analysis results to print to the console in real time.')

# passing arguemtns with files to variables
args = parser.parse_args()
ref_file = args.reference_file
seq_file = args.input_file

# funciton to perform the substitution analysis between a reference and target sequence 
def subAnalysis (ref_seq, input_seq):
    query_length = len(ref_seq) # sequence length
    indexes = '' # holds the indexes of substitutions 
    identity = 0 # holds the identity score
    sub_ct = 0 # holds count for substitutions
    # flow control to compare sequences at each index
    for i in range(query_length - 1):
        # flow control to recognize substitutions
        if input_seq[i] != ref_seq[i]:
            indexes = indexes + i +', ' # store index
            sub_ct = sub_ct + 1 # increase count
    identity = (query_length - sub_ct)/query_length # calculate sequence identity

    return indexes, identity


with open(ref_file, 'r') as file: # read file with reference sequence, only one reference sequence per file
    ref_sequence = '' # varaible to hold reference sequence
    # flow control to handle file if reference sequence expands more than one line
    for line in file:
        ref_seq = ref_seq + line
    
with open('compareSEQ_output.txt', 'a') as output: # create and open output file
    with open(seq_file, 'r') as file: # read input file with sequences, one per line
        seq_ct = 1 # input sequence sount
        for line in file:
            output.write('Sequence Number: ' + seq_ct + '\n') # header to record sequence number 
            analysis = subAnalysis(ref_seq = ref_sequence, input_seq = line) # analyze input sequence agaisnt reference sequence
            # write analysis results to output file
            output.write('Mismatched Indexes: ' + analysis[0] + '\n')
            output.write('Sequence Identity: ' + analysis[1] + '\n') 
            # flow control to determine if results are displayed to the console
            if args.verbose:
                print('Sequence Number: ' + seq_ct)
                print('Found Substitution Indexes:')
                print(analysis[0])
                print('Sequence Identity: ')
                print(analysis[1])
            seq_ct = seq_ct + 1
    # end of file writing of the two input file names as a reference post-analysis         
    output.write('\n' + 'Input Files for Analysis:' + '\n')
    output.write(ref_file)
    output.write(seq_file)


