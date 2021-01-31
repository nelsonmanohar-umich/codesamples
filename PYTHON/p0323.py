class Solution:
    """
    https://leetcode.com/problems/number-of-connected-components-in-an-undirected-graph/
    """
    def countComponents(self, n: int, edges: List[List[int]]) -> int:
        A = {}
        for v, w in edges:
            if v not in A: A[v] = []
            if w not in A: A[w] = []
            A[v].append(w)
            A[w].append(v)

        for v in A:
            A[v] = set(A[v])

        visited = {}
        if n == 0: return 0
        if n == 1: return 1
        if len(edges) == 0: return n

        def neighbors(w):
            if w in A:
                return [u for u in A[w] if u not in visited]
            return []

        def dfs(v, visited={}):
            if v in visited:
                return
            visited[v] = True
            for w in neighbors(v):
                if w not in visited:
                    dfs(w, visited)
            return

        tot = 0
        for v in range(n):
            if v not in visited:
                dfs(v, visited)
                tot += 1

        return tot
