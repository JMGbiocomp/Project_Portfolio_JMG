import argparse
import random

## --- Program Argument Managment --- ##
parser = argparse.ArgumentParser(
    prog='[proteinTranslator]',
    description='tool to predict the translation of DNA or RNA sequences into protein (amino acid) sequences and provide a report file evaluate results',
    epilog=''
)

parser.add_argument('path_to_sequence_file', help = '')
parser.add_argument('sequence_type', choices = ['DNA', 'RNA'], help = '')
parser.add_argument('o', '--output', type = str, default = 'translation_report.txt', help = '')
parser.add_argument('-f', '--fasta', action = 'store_true', help = '')
parser.add_argument('-v', '--verbose', action = 'store_true', help = '')

args = parser.parse_args()
sequence_file = args.path_to_sequence_file
report_file = args.output


## --- Functions --- ##

def seq_complement (sequence):
    """seq_compliment function is a tool to determine the complimentary sequence of a given DNA sequence.  Input: (DNA sequence)"""
    inverse_seq = sequence [::-1]
    complement_seq = '' # predefines the string variable to the complementary DNA sequence
    # For loop to generate random DNA seqeuence complemenet string
    for n in inverse_seq:
        if n == 'A':
            complement_seq = complement_seq + 'T'
        elif n == 'T':
            complement_seq = complement_seq + 'A'
        elif n == 'C':
            complement_seq = complement_seq + 'G'
        elif n == 'G':
            complement_seq = complement_seq + 'C'
    return complement_seq

def transcription (sequence):
    """transcription function is a tool to convert a DNA sequence into its RNA complimentary sequence.  Input: (DNA sequence)"""
    reverse_sequence = sequence [::-1] #inverts the sequence
    RNA_seq = '' # predefine the string variable to hold the RNA sequence
    # For loop to generate a complementary RNA seqeunce to inputted DNA sequence
    for n in reverse_sequence:
        if n == 'A':
            RNA_seq = RNA_seq + 'U'
        elif n == 'T':
           RNA_seq = RNA_seq + 'A'
        elif n == 'C':
           RNA_seq = RNA_seq + 'G'
        elif n == 'G':
           RNA_seq = RNA_seq + 'C'
    return RNA_seq

def ORF_slicer (sequence, codon_size, ORF_num):
    """splits DNA/RNA sequence into codons by one of the ORF; sequence = DNA/RNA seq, ORF_num = ORF to slice, length_adjuster = how to adjust the length of se by ORF (0 for ORF1, 2 for ORF2 & 1 for ORF3)"""
    start = ORF_num - 1 
    end = len(sequence)
    return [sequence[i:i+codon_size] for i in range(start, end, codon_size)]

def translation(ORF_codon_list):
    #dictionary of all possible codons (keys) paried with their repsective single letter amino acid identifier (value)
    aa_codons = {
        'GCU':'A',  #'A' = Alanine
        'GCC':'A',
        'GCA':'A',
        'GCG':'A',
        'CGU':'R',  #'R' = Arginine
        'CGC':'R',
        'CGA':'R',
        'CGG':'R',
        'AGA':'R',
        'AGG':'R',
        'AAU':'N',  #'N' = Asn
        'AAC':'N',
        'GAU':'D',  #'D' = Asp
        'GAC':'D',
        'UGU':'C',  #'C' = Cys
        'UGC':'C',
        'CAA':'Q',  #'Q' = Glutamine
        'CAG':'Q',
        'GAA':'E',  #'E' = Glutamic Acid
        'GAG':'E',
        'GGU':'G',  #'G' = Glycine
        'GGC':'G',
        'GGA':'G',
        'GGG':'G',
        'CAU':'H',  #'H' = Histidine
        'CAC':'H',
        'AUG':'M',  #'M' = Methionine = START
        'AUU':'I',  #'I' = Isoleucine
        'AUC':'I',
        'AUA':'I',
        'CUU':'L',  #'L' = Leuicne
        'CUC':'L',
        'CUA':'L',
        'CUG':'L',
        'UUA':'L',
        'UUG':'L',
        'AAA':'K',  #'K' = Lysine
        'AAG':'K',
        'UUU':'F',  #'F' = Phenylalanine
        'UUC':'F',
        'CCU':'P',  #'P' = Proline
        'CCC':'P',
        'CCA':'P',
        'CCG':'P',
        'UCU':'S',  #'S' = Serine
        'UCC':'S',
        'UCA':'S',
        'UCG':'S',
        'AGU':'S',
        'AGC':'S',
        'ACU':'T',  #'T' = Threonine
        'ACC':'T',
        'ACA':'T',
        'ACG':'T',
        'UGG':'W',  #'W' = Tryptophan
        'UAU':'Y',  #'Y' = Tryosine
        'UAC':'Y',
        'GUU':'V',  #'V' = Valine
        'GUC':'V',
        'GUA':'V',
        'GUG':'V',
        'UAA':'*',  #'*' = STOP
        'UGA':'*',
        'UAG':'*'
    }
    #For loop to identify the start codon for the ORF as well as the start and end indeces
    if 'AUG' in ORF_codon_list:
        for c in ORF_codon_list:
            #flow control for recognizing the START codon and assigning the start and end indeces
            if c == 'AUG':
                start = ORF_codon_list.index(c) #determines start position for translation
                end = len(ORF_codon_list) #determines maximum end postion for translation
                break
            else:
                continue
        protein_seq = ''
        #For loop to translate the ORF to a amino acid sequence; Halts execution if it encounters a STOP codon
        for i in range(start, end):
            codon = ORF_codon_list[i] #assigns the codon in the ORF_codon_list to a variable
            #flow control to recognize the STOP codon or proceed for codon to amino acid translation
            if codon not in aa_codons:
                break
            else:
                if aa_codons[codon] == '*':
                    protein_seq = protein_seq + "*"
                    break #exits the loop and does not include the STOP codon in the protein_seq string
                else:
                    aa = aa_codons[codon]  #assigns the value of the aa_codon dictionary to 'aa' with the current codon as the key  
                    protein_seq = protein_seq + aa #adds single letter amino acid to prtoein_seq string
        return protein_seq
    else:
        return "no start codon present in ORF"
    
def central_dogma (bio_sequence, report_file):
    '''
    '''
    if args.sequence_type == 'DNA':
        comp_sequence = seq_complement(bio_sequence)
        seq_5_prime = transcription(bio_sequence)
        seq_3_prime = transcription(comp_sequence)

    elif args.sequence_type == 'RNA':
        seq_3_prime = bio_sequence
        seq_5_prime = bio_sequence[::-1]
    # Handle Open Reading Frames for 5' seqeunce 
    report_file.write('Possible 5 prime DNA Sequences:' + '/n')
    ORF1 = ORF_slicer(seq_5_prime, 3, 1)
    ORF2 = ORF_slicer(seq_5_prime, 3, 2) 
    ORF3 = ORF_slicer(seq_5_prime, 3, 3)
    protein1 = translation(ORF1)
    protein2 = translation(ORF2)
    protein3 = translation(ORF3)
    report_file.write(protein1 + '\n')
    report_file.write(protein2 + '\n')
    report_file.write(protein3 + '\n')
    # Handle Open Reading Frames for 3' sequences
    report_file.write('Possible 3 prime DNA Sequences:' + '/n')
    ORF1 = ORF_slicer(seq_3_prime, 3, 1)
    ORF2 = ORF_slicer(seq_3_prime, 3, 2) 
    ORF3 = ORF_slicer(seq_3_prime, 3, 3)
    protein1 = translation(ORF1)
    protein2 = translation(ORF2)
    protein3 = translation(ORF3)
    report_file.write(protein1 + '\n')
    report_file.write(protein2 + '\n')
    report_file.write(protein3 + '\n')

def reportGenerator (input_file, report_file):
    with open(report_file, 'w') as report:
        if args.verbose:
            report.write('Nucleic Acid Sequence File:' + '\n')
            report.write(input_file + '\n\n')
            print('Predicting protein sequences from:')
            print(input_file)
            count = 0
            print(str(count))
        with open(input_file, 'r') as input:
            for line in input:
                if args.fasta:
                    if line.startswith('>'):
                        seq_ID = line
                    else:
                        sequence = line
                else:
                    seq_ID = line
                    sequence = line
                report.write('Sequence:' + '\n')
                report.write(seq_ID + '\n')
                central_dogma(sequence, report)
                report.write('\n')
                if args.verbose:
                    print('\r{}'.format(count), end = '')
                    count = count + 1
            report.write('# --- End of Protein Translation Predictions --- #')

## --- Program Script --- ##

reportGenerator(sequence_file, report_file)
