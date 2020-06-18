class Solution:
    '''
    Given a 2d grid map of '1's (land) and '0's (water), count
    the number of islands. An island is surrounded by water and
    is formed by connecting adjacent lands horizontally or
    vertically. You may assume all four edges of the grid are
    all surrounded by water.

    Example 1:
    Input:
       11110
       11010
       11000
       00000
    Output: 1

    Example 2:
    Input:
       11000
       11000
       00100
       00011
    Output: 3
    '''
    def numIslands(self, grid: List[List[str]]) -> int:
        if not len(grid): return 0
        n, m = len(grid), len(grid[0])

        ones = []
        for i in range(n):
            for j in range(m):
                if grid[i][j] == '1':
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
            if grid[i][j] != '1':
                return
            if current in visited:
                if parent != visited[current]:
                    print("ERROR:", current, parent, visited[current], ones, visited)
                return
            visited[current] = parent
            adjs = adjacencies(current)
            for adj in adjs:
                ii, jj = adj
                if grid[ii][jj] == '1':
                    dfs(adj, parent, visited)
            return


        visited = {}
        num_islands = 0
        ports = {}
        while len(ones):
            port = ones.pop()
            dfs(port, port, visited)
            ones = [item for item in ones if item not in visited]
            #print("visited", visited)
            #print("this time", [x for x in visited if visited[x] == port])
            ports[port] = [port]
            num_islands += 1
            #print("-"*32)


        for port in ports:
            ports[port] = [x for x in visited if visited[x] == port]
            #print(port, ports[port])


        return num_islands



