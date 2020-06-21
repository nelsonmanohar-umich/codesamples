class Solution:
    '''
    Given a triangle, find the minimum path sum from top to bottom.
    Each step you may move to adjacent numbers on the row below.
    For example, given the following triangle
    [
         [2],
        [3,4],
       [6,5,7],
      [4,1,8,3]
    ]
    The minimum path sum from top to bottom is 11 (i.e., 2 + 3 + 5 + 1 = 11).
    Note:
    Bonus point if you are able to do this using only O(n) extra space,
    where n is the total number of rows in the triangle.
    '''
    def minimumTotal(self, triangle: List[List[int]]) -> int:
        # it times out on last case, because it needs to be made iterative dfs or instead a dp.
        n = len(triangle)
        if n == 0:
            return None
        if n == 1:
            return triangle[0][0]

        self.minsum = 99999999999999
        def dfs(i, j, lsum):# path):
            if i == n-1:
                if lsum < self.minsum:
                    self.minsum = lsum
                return
            for jj in range(j, j+2):
                val = triangle[i+1][jj]
                dfs(i+1, jj, lsum+val)#, path+[val])
            return

        top = triangle[0][0]
        dfs(0, 0, top)#, [top])
        return self.minsum


