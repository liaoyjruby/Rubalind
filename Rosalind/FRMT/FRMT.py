# Given: A collection of n (n≤10) GenBank entry IDs.
# Return: The shortest of the strings associated with the IDs in FASTA format.
file = "/Users/liaoyj/Documents/Rubalind/Rosalind/FRMT/rosalind_frmt.txt"
with open(file) as f:
    input = f.read().strip().split(" ") 
gbk_ids = input
def FRMT(gbk_ids):
    from Bio import Entrez, SeqIO
    Entrez.email = "liaoyjruby@gmail.com"
    handle = Entrez.efetch(
        db = "nucleotide",
        id = gbk_ids,
        rettype = "fasta")
    records = list(SeqIO.parse(handle, "fasta"))
    id_lens = [len(rec.seq) for rec in records]
    idx = id_lens.index(min(id_lens))
    return records[idx].format("fasta")

out = FRMT(gbk_ids)
print(out)

with open('/Users/liaoyj/Documents/Rubalind/Rosalind/FRMT/FRMT_submission.txt', 'w') as f:
    f.write(out)



