class Solution:
    ''' Given n pairs of parentheses, write a function to generate
        all combinations of well-formed parentheses.

        For example, given n = 3, a solution set is:
            [
            "((()))",
            "(()())",
            "(())()",
            "()(())",
            "()()()"
            ]
    '''
    def generateParenthesis(self, n: int) -> List[str]:

        if n == 0:
            return [""]

        if n == 1:
            return ["()"]


        def dfs(m, path="", paths={}, visited={}):
            key = (path, m)
            if key in visited:
                return
            visited[key] = True

            if m == 0:
                paths[path] = True

            if m < 0:
                return

            def adjacencies(path):
                opening_count = path.count("(")
                closing_count = path.count(")")
                openings = "(" if (n-opening_count) > 0 else ""
                closings = ")" if (opening_count-closing_count) > 0 else ""
                adjs = openings + closings
                #print("***", path, "opening, closing", opening_count, closing_count, openings, closings)
                return adjs

            for bracket in adjacencies(path):
                dfs(m-1, path+bracket, paths, visited)


        paths = {}
        visited = {}
        dfs(2*n, "", paths, visited)
        keys = list(paths.keys())

        return keys
