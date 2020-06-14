class Solution:
    '''
    In a given grid, each cell can have one of three values:
        the value 0 representing an empty cell;
        the value 1 representing a fresh orange;
        the value 2 representing a rotten orange.
    Every minute, any fresh orange that is adjacent (4-directionally) to a rotten orange becomes rotten.
    Return the minimum number of minutes that must elapse until no cell has a fresh orange.  If this is impossible, return -1 instead.

    Example 1:
        Input: [[2,1,1],[1,1,0],[0,1,1]]
        Output: 4
    '''
    def orangesRotting(self, grid: List[List[int]]) -> int:
        n, m = len(grid), len(grid[0])
        if n == 0 and m == 0:
            return -1
        if m == 1 and n==1:
            return 0 if grid[0][0] in (2,0) else -1

        rotten, good = {}, {}
        for i in range(n):
            for j in range(m):
                if grid[i][j] == 2: rotten[(i,j)] = True
                if grid[i][j] == 1: good[(i,j)] = True

        def up(i,j): a = (i-1,j) if (i-1,j) in rotten else (); print(a); return a
        def do(i,j): a = (i+1,j) if (i+1,j) in rotten else (); print(a); return a
        def le(i,j): a = (i,j-1) if (i,j-1) in rotten else (); print(a); return a
        def ri(i,j): a = (i,j+1) if (i,j+1) in rotten else (); print(a); return a

        nmin = 0
        fresh = list(good.keys())
        if not len(rotten):
            return -1

        while len(fresh):
            now_bad = []
            for orange in fresh:
                i, j = orange
                if up(i,j) or do(i,j) or le(i,j) or ri(i,j):
                    now_bad.append(orange)
            fresh = [x for x in fresh if x not in now_bad]
            for orange in now_bad: rotten[tuple(orange)] = True
            if not now_bad:
                if len(fresh):
                    return -1
            nmin += 1

        return nmin


