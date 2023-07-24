# Given: A set of protein strings in FASTA format that share some motif with minimum length 20.
# Return: Regular expression for the best-scoring motif.

rosa_id = "NEED"
file = "/Users/liaoyj/Documents/Rubalind/Rosalind/" + rosa_id + "/NEED_sample.txt"

from Bio import SeqIO
from Bio import Entrez
from Bio.SeqRecord import SeqRecord

with open(file) as f:
    lines = f.readlines()[0].rstrip().split(" ")
    id_1 = lines[0]
    id_2 = lines[1]

# Access from GenBank
Entrez.email = "liaoyjruby@gmail.com"
search = '"' + id_1 + '" OR "' + id_2 + '"'
handle = Entrez.esearch(db="nucleotide", term=search)
record = Entrez.read(handle)

with open("/Users/liaoyj/Documents/Rubalind/Rosalind/BFIL/BFIL_submission.fq", "w") as handle:
    SeqIO.write(seqs_trim, handle, "fastq")