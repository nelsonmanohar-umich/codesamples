class Solution:
    '''
    Given an array of integers A with even length, return true if and only if it is possible
    to reorder it such that A[2 * i + 1] = 2 * A[2 * i] for every 0 <= i < len(A) / 2.
    Example 1:
        Input: [3,1,3,6]
        Output: false
    '''
    def canReorderDoubled(self, A: List[int]) -> bool:
        n = len(A)
        if len(set(A)) == 1:
            if A[0] != 0:
                return False
            return True

        if n % 2:
            return False

        values = sorted(A, key= lambda x : abs(x))

        v2i = {}
        for i, v in enumerate(values):
            if v not in v2i: v2i[v] = []
            v2i[v].append(i)

        in_use = [False] * n
        for i in range(n):
            v = values[i]
            if in_use[i]: continue
            #print("***", (i, v), in_use, v2i[v], v2i[2*v] if 2*v in v2i else None)
            in_use[i] = True
            if v*2 in v2i:
                m = min(len(v2i[v]), len(v2i[2*v]))
                if m > 0:
                    for j in v2i[2*v][:m]:
                        jj = v2i[2*v].pop()
                        in_use[jj] = True
                    for j in v2i[v][:m]:
                        jj = v2i[v].pop()
                        in_use[jj] = True
                else:
                    in_use[i] = False
                    continue
            else:
                in_use[i] = False
                return False

        if any([not in_use[x] for x in range(n)]):
            return False

        return True

