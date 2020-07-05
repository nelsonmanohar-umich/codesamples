class Solution:
    def diagonalSort(self, mat: List[List[int]]) -> List[List[int]]:
        # diag function was rolled out into inlne code, because dropped performance by 50%
        n = len(mat)
        m = len(mat[0]) if n else 0

        def diag(i,j, mat):
            values, indices = [], []
            for ix in range(max(n,m)):
                if i+ix < n and j+ix < m:
                    v = mat[i+ix][j+ix]
                    indices.append((i+ix,j+ix))
                    values.append(v)
                else:
                    break
            values.sort()
            for (ii, jj), v in zip(indices, values):
                mat[ii][jj] = v
            return mat

        for j in range(m):
            i = 0
            values, indices = [], []
            for ix in range(max(n,m)):
                if i+ix < n and j+ix < m:
                    v = mat[i+ix][j+ix]
                    indices.append((i+ix,j+ix))
                    values.append(v)
                else:
                    break
            values.sort()
            for (ii, jj), v in zip(indices, values):
                mat[ii][jj] = v

        for i in range(1,n):
            j = 0
            values, indices = [], []
            for ix in range(max(n,m)):
                if i+ix < n and j+ix < m:
                    v = mat[i+ix][j+ix]
                    indices.append((i+ix,j+ix))
                    values.append(v)
                else:
                    break
            values.sort()
            for (ii, jj), v in zip(indices, values):
                mat[ii][jj] = v

        return mat
