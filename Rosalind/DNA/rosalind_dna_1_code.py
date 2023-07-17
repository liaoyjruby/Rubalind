# Given: A DNA string s of length at most 1000 nt.
# Return: Four integers (separated by spaces) counting the respective number of times 
    # that the symbols 'A', 'C', 'G', and 'T' occur in s.

def nucCount(sequence):
    count = {'A': 0, 'C': 0, 'G': 0, 'T': 0}
    for nuc in sequence:
        count[nuc] += 1
    counts = []
    for nuc in count:
        counts.append(str(count[nuc]))
    return " ".join(counts)