
class Solution:
    ''' Given a matrix of m x n elements (m rows, n columns),
        return all elements of the matrix in spiral order.

    Example 1:
    Input:
    [
     [ 1, 2, 3 ],
     [ 4, 5, 6 ],
     [ 7, 8, 9 ]
    ]
    Output: [1,2,3,6,9,8,7,4,5]
    '''
    def spiralOrder(self, matrix: List[List[int]]) -> List[int]:
        n = len(matrix)
        m = len(matrix[0]) if n else 0

        if n == 0 or m == 0:
            return []
        if n == 1:
            return matrix[0]
        if n == 2:
            return matrix[0] + matrix[1][::-1]

        kmax = n//2 + 1

        vals = []
        for k in range(kmax):
            right = [matrix[k][j] for j in range(k,m-k)]
            if not right: break
            vals.extend(right)

            down = [matrix[i][m-k-1] for i in range(k+1,n-k)]
            if not down: break
            vals.extend(down)

            left = [matrix[n-k-1][j] for j in range(m-k-2,k-1,-1)]
            if not left: break
            vals.extend(left)

            up = [matrix[i][k] for i in range(n-k-2, k, -1)]
            if not up: break
            vals.extend(up)

        return vals

