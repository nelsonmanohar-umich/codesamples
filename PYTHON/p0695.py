class Solution:
    '''
    Given a non-empty 2D array grid of 0's and 1's, an island is a group of 1's
    (representing land) connected 4-directionally (horizontal or vertical.) You
    may assume all four edges of the grid are surrounded by water.

    Find the maximum area of an island in the given 2D array. (If there is no
    island, the maximum area is 0.)

    Example 1:
        [[0,0,1,0,0,0,0,1,0,0,0,0,0],
         [0,0,0,0,0,0,0,1,1,1,0,0,0],
         [0,1,1,0,1,0,0,0,0,0,0,0,0],
         [0,1,0,0,1,1,0,0,1,0,1,0,0],
         [0,1,0,0,1,1,0,0,1,1,1,0,0],
         [0,0,0,0,0,0,0,0,0,0,1,0,0],
         [0,0,0,0,0,0,0,1,1,1,0,0,0],
         [0,0,0,0,0,0,0,1,1,0,0,0,0]]
    Given the above grid, return 6. Note the answer is not 11, because the island
    must be connected 4-directionally.
    '''
    def maxAreaOfIsland(self, grid: List[List[int]]) -> int:
        if not len(grid): return 0
        n, m = len(grid), len(grid[0])

        ones = []
        for i in range(n):
            for j in range(m):
                if grid[i][j] == 1:
                    ones.append((i,j))

        def up(i,j): return (i-1,j) if i-1 >= 0 else ()
        def do(i,j): return (i+1,j) if i+1 < n else ()
        def le(i,j): return (i,j-1) if j-1 >= 0 else ()
        def ri(i,j): return (i,j+1) if j+1 < m else ()

        def adjacencies(current):
            #for row in grid: print(row)
            i, j = current
            adjs = [f(i,j) for f in [up, do, le, ri] if f(i,j)]
            #print("adjs", current, adjs)
            return adjs


        def dfs(current, parent, visited={}):
            i, j = current
            if grid[i][j] != 1:
                return
            if current in visited:
                if parent != visited[current]:
                    print("ERROR:", current, parent, visited[current], ones, visited)
                return
            visited[current] = parent
            adjs = adjacencies(current)
            for adj in adjs:
                ii, jj = adj
                if grid[ii][jj] == 1:
                    dfs(adj, parent, visited)
            return


        visited = {}
        num_islands = 0
        ports = {}
        i_max = 0
        while len(ones):
            port = ones.pop()
            dfs(port, port, visited)
            ones = [item for item in ones if item not in visited]
            #print("visited", visited)
            #print("this time", [x for x in visited if visited[x] == port])
            ports[port] =  [x for x in visited if visited[x] == port]
            num_islands += 1
            if len(ports[port]) > i_max:
                i_max = len(ports[port])
            #print("-"*32)

        #for port in ports:
            #print(port, ports[port])

        return i_max



