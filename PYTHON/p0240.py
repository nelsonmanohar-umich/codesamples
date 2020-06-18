class Solution:
    '''
    Write an efficient algorithm that searches for a value in an m x n
    matrix. This matrix has the following properties:

    Integers in each row are sorted in ascending from left to right.
    Integers in each column are sorted in ascending from top to bottom.

    Example:
    Consider the following matrix:
    [ [1,   4,  7, 11, 15],
      [2,   5,  8, 12, 19],
      [3,   6,  9, 16, 22],
      [10, 13, 14, 17, 24],
      [18, 21, 23, 26, 30] ]
    Given target = 5, return true.
    Given target = 20, return false.
    '''
    def searchMatrix(self, matrix, target):
        """
        :type matrix: List[List[int]]
        :type target: int
        :rtype: bool
        """

        def binsearch(items, target):
            n = len(items)
            lo, hi = 0, n-1
            while lo <= hi:
                mi = lo + (hi-lo)
                v = items[mi]
                if v == target:
                    return True
                if v < target:
                    lo, hi = mi+1, hi
                if v > target:
                    lo, hi = lo, mi-1
            return False

        n = len(matrix)
        if n == 0:
            return False
        n, m = len(matrix), len(matrix[0])
        if m == 0:
            return False
        if n == 1 and m == 1:
            return matrix[0][0] == target
        if n == 1:
            row = matrix[0]
            return binsearch(row, target)
        if m == 1:
            col = [matrix[j][0] for j in range(n)]
            return binsearch(col, target)

        for i in range(n):
            v = matrix[i][m-1]
            if v == target:
                return True
            if v > target:
                row = matrix[i]
                if binsearch(row, target):
                    return True

        for j in range(m):
            v = matrix[n-1][j]
            if v == target:
                return True
            if v > target:
                col = [matrix[i][j] for i in range(n)]
                if binsearch(col, target):
                    return True

        return False



