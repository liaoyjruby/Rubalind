# Given: FASTQ file, quality cut-off value q, Phred33 quality score assumed.
# Return: FASTQ file trimmed from the both ends (removed leading and trailing bases with quality lower than q)

rosa_id = "BFIL"
file = "/Users/liaoyj/Documents/Rubalind/Rosalind/" + rosa_id + "/BFIL_sample.txt"

from io import StringIO
from Bio import SeqIO
import numpy as np
import pandas as pd

with open(file) as f:
    lines = f.readlines()
    q = int(lines[0].strip())
    with StringIO("".join(lines[1:])) as fq_io:
        seqs = SeqIO.parse(fq_io, "fastq")
        for seq in seqs:
            phreds_lq = np.array(seq.letter_annotations["phred_quality"]) < q
            phreds_lq_idx = [i for i, x in enumerate(phreds_lq) if x] # positions of low quality bases
            # Leading trim
            if(phreds_lq_idx[0] == 0):
                ct_lead = 0


            # Trailing trim
            if(phreds_lq_idx[-1] == len(phreds_lq) - 1):
                

    df_phred = pd.DataFrame(lst_phred)
    pos_mean = (df_phred.mean() < q).sum() # mean base quality falls below given threshold
    print(pos_mean)