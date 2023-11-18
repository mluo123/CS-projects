from sys import argv

#open codon sequence dictionary file and put it into an array
arraysplit = [] 
for line in open(argv[2], "r"):
    arraysplit.append(line.strip().split(" "))

#create dictionary and put codons corresponding to their letter
codons = {}
for row in arraysplit:
    if row != ['']:
        codons[row[0]] = row[1]

#create string of all of the nucleotides in the fq file
nucleotides = ""
for genes in open(argv[1], "r"):
    if genes.startswith(">") == False:
        for letter in genes:
            if letter != " " and letter != "\n":
                nucleotides += letter.upper()

#establish start and stop codons
start = "ATG"
stop1 = "TGA"
stop2 = "TAA"
stop3 = "TAG"

#loop through nucleotide sequence
i = 0
while i < len(nucleotides)-3:
    codon = nucleotides[i:i+3] #establish codon
    if codon == start: #if start codon, start creating a string
        seq = ""
        j = i
        while j < len(nucleotides)-3:
            codon1 = nucleotides[j:j+3] #establish codon again
            if codon1 == stop1 or codon1 == stop2 or codon1 == stop3: #if stop codon, print seq, else continue adding to string
                break
            else:
                seq += codons[codon1]
                j += 3
        print(seq)
    i += 1 #advance one letter in nucleotide sequence

#create reverse sequence from original nucleotide sequence
reverse = ""
for letter in nucleotides[::-1]:
    if letter == "A":
        reverse += "T"
    elif letter == "T":
        reverse += "A"
    elif letter == "C":
        reverse += "G"
    else:
        reverse += "C"

#do the same printing process for reverse string as we did earlier
m = 0
while m < len(reverse)-3:
    codon = reverse[m:m+3]
    if codon == start:
        seq = ""
        k = m
        while k < len(reverse)-3:
            codon1 = reverse[k:k+3]
            if codon1 == stop1 or codon1 == stop2 or codon1 == stop3:
                break
            else:
                seq += codons[codon1]
                k += 3
        print(seq)
    m += 1
