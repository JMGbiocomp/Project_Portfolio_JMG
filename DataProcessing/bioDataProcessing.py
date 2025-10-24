import argparse

parser = argparse.ArgumentParser(
    prog='biodata_processing.py',
    description="Processes input file of high throughput biolgical data to remove missing data, replace or remove NAs, identify errors and flag outliers for review",
    epilog= "Data set had been processed and output file is ready for downstream analysis",
)

# program arguments 
parser.add_argument('inputfile', help="path to data file (text or csv)")
parser.add_argument('output_name', help='path to output file', default = 'output')
parser.add_argument('-d', '--datatype', choices=['micro', 'RNAseq', 'scRNAseq', 'prot', 'metab'], required=True, help='specifies the type of -omics level data')



args = parser.parse_args()
print(args)
inputfile = args.inputfile
output_name = args.output_name

import numpy as np
import pandas as pd
import re