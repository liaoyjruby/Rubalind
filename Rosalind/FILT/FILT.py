# Given: A quality threshold value q, percentage of bases p, and set of FASTQ entries.
# Return: Number of reads in filtered FASTQ entries

file = "/Users/liaoyj/Documents/Rubalind/Rosalind/FILT/rosalind_filt.txt"

from io import StringIO
from Bio import SeqIO
import numpy as np

with open(file) as f:
    lines = f.readlines()
    q, p = [int(n) for n in lines[0].strip().split(" ")]
    with StringIO("".join(lines[1:])) as fq_io:
        seqs = SeqIO.parse(fq_io, "fastq")
        n_filt = 0
        for seq in seqs:
            quals = np.array(seq.letter_annotations["phred_quality"])
            n_q = (quals >= q).sum() # ct of reads above quality threshold
            # pct = np.percentile(quals, 100-p)
            # print(n_q / len(quals))
            if(n_q / len(quals) >= p/100):
                n_filt += 1
    print(n_filt)