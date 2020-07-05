class Solution:
    '''
    Given a 2D binary matrix filled with 0's and 1's, find the largest rectangle containing only 1's and return its area.
    Example:
      Input:
      [
        ["1","0","1","0","0"],
        ["1","0","1","1","1"],
        ["1","1","1","1","1"],
        ["1","0","0","1","0"]
      ]
      Output: 6
    '''
    def maximalRectangle(self, matrix: List[List[str]]) -> int:
        # yes, i know it is optimal
        def intersect(v1, v2, u):
            wsums = [vv1+vv2 if vv1 * vv2 else 0 for vv1, vv2 in zip(u, v2)]
            return wsums

        def maxval(u):
            rsum, rmax = 0, 0
            #w = []
            for uu in u:
                if uu != 0:
                    rsum += uu
                    if rsum > rmax:
                        rmax = rsum
                else:
                    if rsum > rmax: rmax = rsum
                    rsum = 0
                #w.append(rsum)
            return rmax

        def colrow(j):
            w = [M[i][j] for i in range(n)]
            return w

        M = matrix
        n = len(M)
        m = len(M[0]) if n else 0

        for i in range(n):
            for j in range(m):
                M[i][j] = int(M[i][j])

        if n == 0 or m==0:
            return 0

        if n == 1:
            a = maxval(M[0])
            return a

        if m == 1:
            a = maxval([M[i][0] for i in range(n)])
            return a

        self.max = 0
        for i in range(n):
            w = M[i]
            a = maxval(w)
            if a > self.max:
                self.max = a
            for j in range(i+1,n):
                w = intersect(M[i], M[j], w)
                b = maxval(w)
                if b == 0: break
                if b > self.max:
                    self.max = b

        for i in range(m):
            w = colrow(i)
            a = maxval(w)
            if a > self.max:
                self.max = a
            for j in range(i+1,m):
                u, v = colrow(i), colrow(j)
                w = intersect(u, v, w)
                b = maxval(w)
                if b == 0: break
                if b > self.max:
                    self.max = b

        return self.max


