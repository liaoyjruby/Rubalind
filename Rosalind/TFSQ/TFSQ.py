# Given: FASTQ file
# Return: Corresponding FASTA records

file = "/Users/liaoyj/Documents/Rubalind/Rosalind/TFSQ/rosalind_tfsq.fq"
def TFSQ(fq):
    from Bio import SeqIO
    fq = SeqIO.parse(file, "fastq")
    with open("/Users/liaoyj/Documents/Rubalind/Rosalind/TFSQ/TFSQ_submission.fasta", "w") as handle:
        SeqIO.write(fq, handle, "fasta")

TFSQ(file)

