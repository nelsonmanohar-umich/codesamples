class Solution:
    """
    https://leetcode.com/problems/number-of-provinces/
    There are n cities. Some of them are connected, while
    some are not. If city a is connected directly with city b,
    and city b is connected directly with city c, then city a
    is connected indirectly with city c.

    A province is a group of directly or indirectly connected
    cities and no other cities outside of the group.

    You are given an n x n matrix isConnected where
    isConnected[i][j] = 1 if the ith city and the jth city
    are directly connected, and isConnected[i][j] = 0 otherwise.

    Return the total number of provinces.
    """
    def findCircleNum(self, isConnected: List[List[int]]) -> int:
        def neighbors(v):
            adj = [i for i, x in enumerate(isConnected[v]) if x]
            return adj

        # generates the reachability set of v
        def visit(v, visited):
            if v in visited: return
            visited[v] = True
            for neighbor in neighbors(v):
                if neighbor not in visited:
                    visit(neighbor, visited)
            return

        visited = {}

        n = len(isConnected)
        np = 0
        for v in range(n):
            if v in visited: continue
            if len(visited) == n: break
            visit(v, visited)
            np += 1

        return np
