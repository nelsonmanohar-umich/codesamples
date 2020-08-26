class Solution:
    '''
    A string S of lowercase English letters is given. We want to partition
    this string into as many parts as possible so that each letter appears
    in at most one part, and return a list of integers representing the size
    of these parts.
    '''
    def partitionLabels(self, S: str) -> List[int]:
        n = len(S)


        def build(S):
            R = {}
            v2i = {}
            for i, v in enumerate(S):
                if v not in v2i: v2i[v] = []
                v2i[v].append(i)
            for s in v2i:
                if s not in R:
                    lo, hi = min(v2i[s]), max(v2i[s])
                    R[s] = [lo, hi]
            return R


        def scan(S):
            parts = []
            i, s, e = 0, 0, 1
            R = build(S)
            while i < n:
                c = S[i]
                ss, ee = R[c]
                s, e = max(s, ss), max(e, ee)
                j = s
                while j < e+1:
                    d = S[j]
                    sss, eee = R[d]
                    ss, ee = min(ss, sss), max(ee, eee)
                    j += 1
                    e = ee
                parts.append(e-s+1)
                i, s, e = ee + 1, ee + 1, ee + 2
            return parts

        parts = scan(S)

        return parts


