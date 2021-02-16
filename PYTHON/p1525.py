class Solution:
    """
    https://leetcode.com/problems/number-of-good-ways-to-split-a-string/
    """
    def numSplits(self, s: str) -> int:
        n = len(s)
        if n <= 1: return 0
        #if len(set(s)) == 1: return n-1

        def windowed_cnt():
            tot = 0
            lc, rc = Counter(s[:1]), Counter(s[1:])
            if len(lc) == len(rc): tot += 1

            for i in range(2,n):
                c = s[i-1]
                rc[c] -= 1
                if rc[c] == 0: del rc[c]
                if c not in lc: lc[c] = 0
                lc[c] += 1
                if len(lc) == len(rc): tot += 1
            return tot

        return windowed_cnt()


        def brute_force():
            tot = 0
            for i in range(2,n):
                left, right = s[:i], s[i:]
                sl, sr = set(left), set(right)
                if len(sl) == len(sr):
                    tot += 1
            return tot

        return brute_force()


