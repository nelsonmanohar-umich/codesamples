class Solution:
    '''
    You are given a map in form of a two-dimensional integer
    grid where 1 represents land and 0 represents water.

    Grid cells are connected horizontally/vertically (not
    diagonally). The grid is completely surrounded by water,
    and there is exactly one island (i.e., one or more
    connected land cells).

    The island doesn't have "lakes" (water inside that
    isn't connected to the water around the island). One
    cell is a square with side length 1. The grid is
    rectangular, width and height don't exceed 100.
    Determine the perimeter of the island.

    Example:
    Input:
    [[0,1,0,0],
     [1,1,1,0],
     [0,1,0,0],
     [1,1,0,0]]
    Output: 16
    '''
    def islandPerimeter(self, grid: List[List[int]]) -> int:
        n, m = len(grid), len(grid[0])

        self.perim = 0
        def up(i,j): return (i-1,j) if i-1>=0 and grid[i-1][j] else ()
        def do(i,j): return (i+1,j) if i+1<n and grid[i+1][j] else ()
        def ri(i,j): return (i,j+1) if j+1<m and grid[i][j+1] else ()
        def le(i,j): return (i,j-1) if j-1>=0 and grid[i][j-1] else ()
        def neighbors(i,j): return [f(i,j) for f in (up, do, ri, le) if f(i,j)]

        for i in range(n):
            for j in range(m):
                if grid[i][j] == 0: continue
                self.perim += (4 - len(neighbors(i,j)))
        return self.perim

        def dfs(current, visited={}):
            if current in visited: return
            i,j = current
            visited[current] = True
            self.perim += (4 - len(neighbors(i,j)))
            for adj in neighbors(i,j):
                dfs(adj, visited)
            return

        for i in range(n):
            for j in range(m):
                if grid[i][j]:
                    start = (i,j)
                    break

        dfs(start, {})

        return self.perim





