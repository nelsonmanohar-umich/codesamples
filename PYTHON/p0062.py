class Solution:
    # https://leetcode.com/problems/unique-paths/
    def uniquePaths(self, m: int, n: int) -> int:
        self.n = 0
        ways = [[0 for j in range(m)] for i in range(n)]
        def fill(i,j, ways):
            if i > 0 and j > 0:
                ways[i][j] = ways[i-1][j] + ways[i][j-1]
            if i > 0 and j == 0:
                ways[i][j] = 1
            if i == 0 and j > 0:
                ways[i][j] = 1
            if i == 0 and j == 0:
                ways[i][j] = 1
            return ways

        for i in range(n):
            for j in range(m):
                ways = fill(i, j, ways)

        return ways[n-1][m-1]



        def brute_force():
            def reached(i,j):
                return i==n-1 and j==m-1

            def dfs(i,j):
                if reached(i,j):
                    self.n += 1
                    return
                if i+1 < n:
                    dfs(i+1, j)
                if j+1 < m:
                    dfs(i, j+1)
                return

            dfs(0, 0)
            return self.n



