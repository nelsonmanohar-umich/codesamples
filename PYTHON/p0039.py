class Solution:
    ''' Given a set of candidate numbers (candidates) (without
        duplicates) and a target number (target), find all unique
        combinations in candidates where the candidate numbers
        sums to target.

        The same repeated number may be chosen from candidates
        unlimited number of times.

        Note:

        All numbers (including target) will be positive integers.
        The solution set must not contain duplicate combinations.
    '''
    def combinationSum(self, candidates: List[int], target: int) -> List[List[int]]:

        n = len(candidates)
        if n == 0:
            return []

        if target == 0:
            return []

        if target == 1:
            return [[1]] if 1 in candidates else []

        if n == 1:
            num = candidates[0]
            if target >= num and target % num == 0:
                return [[num]* (target//num)]
            else:
                return []


        def dfs(goal, path=[], paths={}, visited={}):
            key = (tuple(path), goal)
            if key in visited:
                return
            visited[key] = True

            if goal == 0:
                paths[tuple(sorted(path))] = True
                return

            adjacencies = [c for c in candidates if c <= goal]
            for cand in adjacencies:
                dfs(goal-cand, path+[cand], paths, visited)
            return


        paths = {}
        visited = {}
        dfs(target, [], paths, visited)
        return list(paths.keys())
