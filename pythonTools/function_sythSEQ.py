import argparse
import random

## --- Program Argument Managment --- ##
parser = argparse.ArgumentParser(
    prog='sythSEQ',
    description='generates sythetic biological sequences',
    epilog=''
)

parser.add_argument('sequence_type', type = str, choices=['DNA', 'RNA', 'protein'], help = 'Positional argument (1st) to specifiy which type of bioligical sequences will be generated into an output file')
parser.add_argument('sequence_length', type =int, help = 'Required integer positional argument (2nd) for the length of the generated sequences')
parser.add_argument('sequence_count', type = int, default = 1, help = 'Regired integer positional argument (3rd) to determine the number of sequences to generate')
parser.add_argument('output_file_path', help = 'Positional argument (4th) for output file path')
parser.add_argument('-f', '--fasta', action = 'store_true', help = 'flag to indicate if the output file will be in fasta format')

args = parser.parse_args()
seq_type = args.sequence_type
seq_length = args.sequence_length
seq_count = args.sequence_count
seq_file = args.output_file_path

## --- Program Functions --- ##

def seqGenerator (type, length, count, report_file):
    ID_bases = ['1','2','3','4','5','6','7','8','9','0','B','J','O','U','Z']

    if type == 'DNA':
        bases = ['A','T','C','G']
    elif type == 'RNA':
        bases = ['A','U','C','G']
    else:
        bases = ['A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y']
    with open(report_file, 'w') as report:
            for i in range(count):
                sequence = "".join(random.choices(bases, k = length))
                if args.fasta:
                    seq_ID = '>' + '00000' "".join(random.sample(ID_bases, k = 8))
                    report.write(seq_ID + '\n')
                report.write(sequence)
                if i != count - 1:
                     report.write('\n')

## --- Program Script --- ##

seqGenerator(seq_type, seq_length, seq_count, seq_file)