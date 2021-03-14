class Solution:
    """
    https://leetcode.com/problems/restore-ip-addresses/
    """
    def restoreIpAddresses(self, s: str) -> List[str]:
        def valid(s):
            if len(s) > 3: return False
            if len(s) > 1 and s.startswith('0'): return False
            return len(s) and 0 <= int(s) <= 255

        # backtracking
        def dfs(rem, path="", paths={}, visited={}):
            key = (path, rem)
            if key in visited: return
            visited[key] = 1
            nfields = len(path.split("."))
            if nfields > 4: return
            if not rem:
                if nfields == 4:
                    paths[path] = path
                return

            tv = visited.copy()
            for i in range(1,4):
                cand = rem[:i]
                if cand and valid(cand):
                    tp = path+"."+cand if path else cand
                    key = (cand, tp)
                    dfs(rem[i:], tp, paths, tv)
            return

        visited = {}
        paths = {}
        dfs(s, "", paths, visited)
        return [key for key in paths]


