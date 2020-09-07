class Solution:
    '''
    Given a m * n matrix mat and an integer K, return a matrix answer where
    each answer[i][j] is the sum of all elements
    mat[r][c] for i - K <= r <= i + K, j - K <= c <= j + K, and (r, c)
    is a valid position in the matrix.
    '''
    def matrixBlockSum(self, mat: List[List[int]], K: int) -> List[List[int]]:
        #from functools import lru_cache

        k = K
        n, m = len(mat), len(mat[0]) if len(mat) else 0
        self.rowcache, self.colcache = {}, {}

        def partials():
            #@lru_cache
            def getrowsum(mat, i,j,k):
                if (i, j-1) in self.rowcache:
                    self.rowcache[(i,j)] = self.rowcache[(i,j-1)] \
                                           - (mat[i][j-k-1] if j-k-1 >= 0 else 0) \
                                           + (mat[i][j+k] if j+k < m  else 0)
                else:
                    miny, maxy = max(j-k,0), min(j+k+1,m)
                    self.rowcache[(i,j)] = sum([mat[i][jj] for jj in range(miny, maxy)])
                return self.rowcache[(i,j)]

            #@lru_cache
            def getcolsum(ans,i,j,k):
                if (i-1, j) in self.colcache:
                    self.colcache[(i,j)] = self.colcache[(i-1,j)] \
                                           - (ans[i-k-1][j] if i-k-1 >= 0 else 0) \
                                           + (ans[i+k][j] if i+k < n  else 0)
                else:
                    minx, maxx = max(i-k,0), min(i+k+1,n)
                    self.colcache[(i, j)] = sum([ans[ii][j] for ii in range(minx, maxx)])
                return self.colcache[(i, j)]

            ans = [[0 for j in range(m)] for i in range(n)]
            for i in range(n):
                for j in range(m):
                    ans[i][j] = getrowsum(mat, i,j,k)

            res = [[0 for j in range(m)] for i in range(n)]
            for i in range(n):
                for j in range(m):
                    res[i][j] = getcolsum(ans,i,j,k)
            return res
        return partials()


        def brute_force():
            def kernel(i, j, k):
                minx, maxx = max(i-k,0), min(i+k+1, n)
                miny, maxy = max(j-k,0), min(j+k+1, m)
                s = 0
                for ii in range(minx, maxx):
                    for jj in range(miny, maxy):
                        s += mat[ii][jj]
                return s

            ans = [[0 for j in range(m)] for i in range(n)]
            for i in range(n):
                for j in range(m):
                    ans[i][j] = kernel(i,j, k)
            return ans

        return brute_force()
