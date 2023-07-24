# Given: FASTQ file, quality cut-off value q, Phred33 quality score assumed.
# Return: FASTQ file trimmed from the both ends (removed leading and trailing bases with quality lower than q)

rosa_id = "BFIL"
file = "/Users/liaoyj/Documents/Rubalind/Rosalind/" + rosa_id + "/rosalind_bfil.txt"

from io import StringIO
from Bio import SeqIO
from Bio.SeqRecord import SeqRecord


seqs_trim = []
with open(file) as f:
    lines = f.readlines()
    q = int(lines[0].strip())
    with StringIO("".join(lines[1:])) as fq_io:
        seqs = SeqIO.parse(fq_io, "fastq")
        for seq in seqs:
            phreds = seq.letter_annotations["phred_quality"]
            # phreds_lq_idx = [i for i, x in enumerate(phreds_lq) if x] # positions of low quality bases
            # Leading trim
            lead_trim = 0
            if phreds[lead_trim] < q:
                while phreds[lead_trim] < q:
                    lead_trim += 1
            # Trailing trim
            trail_trim = len(phreds) - 1
            if phreds[trail_trim] < q:
                while phreds[trail_trim-1] < q:
                    trail_trim -= 1
            trim = [lead_trim] + [trail_trim]
            anno = {"phred_quality": seq.letter_annotations["phred_quality"][lead_trim:trail_trim]}
            seq_trim = SeqRecord(
                seq.seq[lead_trim:trail_trim],
                id = seq.id,
                name = seq.name,
                description = seq.description,
                letter_annotations = anno
            )
            seqs_trim.append(seq_trim)

with open("/Users/liaoyj/Documents/Rubalind/Rosalind/BFIL/BFIL_submission.fq", "w") as handle:
    SeqIO.write(seqs_trim, handle, "fastq")