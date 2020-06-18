class Solution:
    ''' Given a collection of distinct integers, return all possible permutations.
        Example:

        Input: [1,2,3]
        Output:
        [
          [1,2,3],
          [1,3,2],
          [2,1,3],
          [2,3,1],
          [3,1,2],
          [3,2,1]
        ]
    '''
    def permute(self, nums: List[int]) -> List[List[int]]:

        n = len(nums)

        def helper(nums, ii, path=[], perms={}, visited={}):
            key = tuple(path)
            #if key in visited: return
            #visited[key] = True

            if len(path) == n:
                perms[key] = len(path)

            neighbors = [i for i in nums if i not in path]
            for neighbor in neighbors:
                helper(nums, neighbor, path+[neighbor], perms, visited)

        perms, visited = {}, {}
        for num in nums:
            helper(nums, num, [num], perms, visited)


        res = []
        for key in perms:
            res.append(list(key))

        return sorted(res)
