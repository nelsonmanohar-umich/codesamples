class Solution:
    '''
    All DNA is composed of a series of nucleotides abbreviated
    as A, C, G, and T, for example: "ACGAATTCCG". When studying
    DNA, it is sometimes useful to identify repeated sequences
    within the DNA.

    Write a function to find all the 10-letter-long sequences
    (substrings) that occur more than once in a DNA molecule.

    Example:
    Input: s = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"
    Output: ["AAAAACCCCC", "CCCCCAAAAA"]
    '''
    def findRepeatedDnaSequences(self, s: str) -> List[str]:
        sequences = {}
        n = len(s)
        res = {}

        for i in range(n-10+1):
            ss = s[i:i+10]
            #print(n, s, ss)
            #if len(ss)!= 10: continue
            if ss not in sequences: sequences[ss] = 0
            sequences[ss] += 1
            if sequences[ss] > 1:
                res[ss] = True

        return res.keys()
