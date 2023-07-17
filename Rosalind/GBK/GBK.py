# "Anthoxanthum"[Organism] AND ("2003/07/25"[PDAT] : "2005/12/27"[PDAT])
file = "/Users/liaoyj/Documents/Rubalind/Rosalind/GBK/rosalind_gbk.txt"
with open(file) as f:
    input = f.read().splitlines() 
genus, start, end = input
def GBK(genus, start, end):
    from Bio import Entrez
    Entrez.email = "liaoyjruby@gmail.com"
    search = '"' + genus + '"[Organism] AND ("' + start + '"[PDAT] : "' + end + '"[PDAT])'
    handle = Entrez.esearch(db="nucleotide", term=search)
    record = Entrez.read(handle)
    return record["Count"]

print(GBK(genus, start, end))