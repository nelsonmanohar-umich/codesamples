class Solution:
    # https://leetcode.com/problems/minimum-increment-to-make-array-unique/
    def minIncrementForUnique(self, A: List[int]) -> int:
        n = len(A)
        if n <= 1: return 0

        amin = min(A)
        amax = max(A)

        values = {}
        i2v = {}
        j = amin
        conflict = {}
        for i, v in enumerate(A):
            if v not in values:
                values[v] = i
            else:
                if v not in conflict: conflict[v] = 0
                conflict[v] += 1

        available = []
        for i in range(amin, amax+n+1):
            if i not in values:
                available.append(i)
        available.reverse()

        tot = 0
        for i in range(amin, amax+1):
            if i in conflict:
                while conflict[i] > 0:
                    free = available.pop()
                    if free > i:
                        tot += free - i
                        conflict[i] -= 1
        return tot



