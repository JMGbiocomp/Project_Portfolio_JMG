import argparse
import random

## --- Program Argument Managment --- ##
parser = argparse.ArgumentParser(
    prog='mutatorSEQ',
    description='',
    epilog=''
)

parser.add_argument('reference_file_path', help = 'required positional argument (1st) of the file path of the reference sequence file')
parser.add_argument('sequence_type', type = str, choices = ['DNA','RNA', 'protein'], help = 'string postional argument to idicate the type of biological sequence being mutated')
parser.add_argument('new_seq_count', type = int, default = 1, help = 'integer positional argument for the number of new seqeucnes mutated from a reference sequence')
parser.add_argument('mutation_count', type = int, default = 5, help = 'integer position argument for ther number of point/missense mutations created from the reference sequence')
parser.add_argument('-o', '--output', choices = ['fasta', 'txt'], default = 'fasta', help = 'optional flag to designate the output file format with the mutated sequences')

args = parser.parse_args()
reference_file = args.reference_file_path
seq_count = args.new_seq_count
mut_count = args.mutation_count

## --- Program Functions --- ##

def mutator (reference, type, count):
    '''
    mutator function desinged to mutate a reference sequence by randomly selecting indexes and mutation types
    reference: reference sequence
    type = biological sequence type
    count = number of mutations to create in reference sequence
    '''
    mutation_choices = ['substitution', 'deletion', 'insertion'] # mutation types
    # flow control to provide lists to generate mutations based on sequence type
    if type == 'DNA':
        bases = ['A','T','C','G']
    elif type == 'RNA':
        bases = ['A','U','C','G']
    else: # protein sequence (AA)
        bases = ['A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y']
    indexes = random.sample(range(len(reference)), k = count) # selects indexes to mutate at random
    # flow control to mutate sequence at selected indexes
    for i in indexes:
        mutation_type = random.choice(mutation_choices) # chooses a mutation type at random
        if mutation_type == 'substitution':
            mutation = random.choice(bases)
            reference[i] = mutation
        elif mutation_type == 'deletion':
            reference = reference[:i] + reference[i+1:]
        else: # insertion mutation
            mutation = random.choice(bases)
            reference = reference[:i] + mutation + reference[i:]
    return reference # returns mutated reference sequence

## --- Program Script --- ##
 # flow control to determine file format
if args.output == 'fasta':
    output_file = 'mutator_output.fasta'
else:
    output_file = 'mutator_output.txt'
# flow control to write an output file with the new mutated sequences
with open(output_file, 'w') as output:
    with open(reference_file, 'r') as r_file:
        for line in r_file:
            # flow control to handle fasta format (headers)
            if line.startswith('>'):
                ref_ID = line
            else:
                ref_seq = line
                # flow control to run mutator() function and write output sequences to file
                for i in range(seq_count):
                    new_seq = mutator(ref_seq, args.sequence_type, mut_count)
                    if args.output == 'fasta':
                        seq_ID = ref_ID + '_' + "".join(random.choices(range(10), k = 10))
                        output.write(seq_ID + '\n')
                    output.write(new_seq)
                    # flow control to move to next line if another sequence will be generated
                    if i != seq_count - 1:
                        output.write('\n')
