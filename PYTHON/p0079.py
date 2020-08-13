class Solution:
    '''
    Given a 2D board and a word, find if the word exists in the grid.
    The word can be constructed from letters of sequentially adjacent
    cell, where "adjacent" cells are those horizontally or vertically
    neighboring. The same letter cell may not be used more than once.
    '''
    def exist(self, board: List[List[str]], word: str) -> bool:
        B = board
        n = len(board)
        m = len(board[0] if n else 0)

        if n == 0: return -1
        if n == 1: return word in "".join(board[0]) or word in "".join(board[0][::-1])
        if m == 0: return -1

        def adjs(i,j,looking_for=""):
            c = looking_for
            a = []
            if j+1 < m and B[i][j+1] == c: a.append((i,j+1))
            if j-1 >= 0 and B[i][j-1] == c: a.append((i,j-1))
            if i+1 < n and B[i+1][j] == c: a.append((i+1,j))
            if i-1 >= 0 and B[i-1][j] == c: a.append((i-1,j))
            return a

        def dfs(word, i, j, visited={}):
            key = (i, j)
            if key in visited: return
            visited[key] = B[i][j]
            if not word:
                #print("FOUND")
                self.found = True
                return

            candidates = adjs(i, j, looking_for=word[0])
            for candidate in candidates:
                ii, jj = candidate
                dfs(word[1:], ii, jj, visited.copy())
                if self.found: return
            return

        v2i = {}
        for i in range(n):
            for j in range(m):
                a = B[i][j]
                if a not in v2i:
                    v2i[a] = []
                v2i[a].append([i,j])

        for c in word:
            if c not in v2i: return False

        c, visited, self.found = word[0], {}, False
        for start in v2i[c]:
            i, j = start
            dfs(word[1:], i, j, visited={})
            if self.found: return True

        return False




