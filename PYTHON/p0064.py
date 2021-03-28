class Solution:
    # https://leetcode.com/problems/minimum-path-sum/
    def minPathSum(self, grid: List[List[int]]) -> int:
        n = len(grid)
        m = len(grid[0]) if n else 0

        ways = grid # [[0 for j in range(m)] for i in range(n)]

        def fill(i, j, ways):
            if i == 0 and j == 0:
                ways[i][j] = grid[i][j]
            if i == 0 and j > 0:
                ways[i][j] = ways[i][j-1] + grid[i][j]
            if i > 0 and j == 0:
                ways[i][j] = ways[i-1][j] + grid[i][j]
            if i > 0 and j > 0:
                ways[i][j] = min(ways[i][j-1], ways[i-1][j]) + grid[i][j]
            return ways

        for i in range(n):
            for j in range(m):
                if i == 0 and j == 0:
                    ways[i][j] = grid[i][j]
                if i == 0 and j > 0:
                    ways[i][j] = ways[i][j-1] + grid[i][j]
                if i > 0 and j == 0:
                    ways[i][j] = ways[i-1][j] + grid[i][j]
                if i > 0 and j > 0:
                    ways[i][j] = min(ways[i][j-1], ways[i-1][j]) + grid[i][j]
        return ways[n-1][m-1]

