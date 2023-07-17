# Given: A quality threshold, along with FASTQ entries for multiple reads.
# Return: The number of reads whose average quality is below the threshold.

file = "/Users/liaoyj/Documents/Rubalind/Rosalind/PHRE/rosalind_phre.txt"

from io import StringIO
from Bio import SeqIO

def mean(list):
    return(sum(list) / len(list))

with open(file) as f:
    lines = f.readlines()
    threshold = int(lines[0].strip())
    n_below = 0
    with StringIO("".join(lines[1:])) as fq_io:
        seqs = SeqIO.parse(fq_io, "fastq")
        for seq in seqs:
            # print("%s %s" % (seq.id, seq.seq))
            avg_qual = mean(seq.letter_annotations["phred_quality"])
            if(avg_qual < threshold):
                n_below += 1
    print(n_below)