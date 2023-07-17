# Given: FASTQ file, quality threshold q
# Return: Number of positions where mean base quality falls below given threshold

rosa_id = "BPHR"
file = "/Users/liaoyj/Documents/Rubalind/Rosalind/" + rosa_id + "/rosalind_bphr.txt"

from io import StringIO
from Bio import SeqIO
import pandas as pd

with open(file) as f:
    lines = f.readlines()
    q = int(lines[0].strip())
    with StringIO("".join(lines[1:])) as fq_io:
        seqs = SeqIO.parse(fq_io, "fastq")
        n_filt = 0
        lst_phred = list()
        for seq in seqs:
            phreds = seq.letter_annotations["phred_quality"]
            lst_phred.append(phreds)
    df_phred = pd.DataFrame(lst_phred)
    pos_mean = (df_phred.mean() < q).sum() # mean base quality falls below given threshold
    print(pos_mean)