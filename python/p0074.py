class Solution:
    ''' Write an efficient algorithm that searches for
        a value in an m x n matrix. This matrix has the
        following properties:

        Integers in each row are sorted from left to right.
        The first integer of each row is greater than the
        last integer of the previous row.
        Example 1:

        Input:
        matrix = [
          [1,   3,  5,  7],
          [10, 11, 16, 20],
          [23, 30, 34, 50]
        ]
        target = 3
        Output: true
    '''
    def searchMatrix(self, matrix: List[List[int]], target: int) -> bool:

        n = len(matrix)
        if n == 0:
            return False

        n, m = len(matrix), len(matrix[0])
        if m == 0:
            return False
        if m == 1:
            return target in [matrix[i][0] for i in range(n)]
        if n == 1:
            return target in matrix[0]

        def pos2index(pos):
            i, j = pos // m, pos % m
            return (i, j)

        N = n*m
        lo, hi = 0, N-1

        while lo <= hi:
            mi = lo + (hi-lo)//2
            i, j = pos2index(mi)
            val = matrix[i][j]
            if val == target:
                return True
            if val < target:
                lo, hi = mi+1, hi
            if val > target:
                lo, hi = lo, mi-1
        return False


