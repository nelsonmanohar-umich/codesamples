class Solution:
    def lengthOfLIS(self, nums: List[int]) -> int:
        self.maxlen = 0
        n = len(nums)

        def lis(nums):
            # LIS-DP, pending, to be done tomorrow
            pass

        def dfs(nums, path=(), paths={}, visited={}):
            key = (nums, path)
            if key in visited: return
            visited[key] = True

            if len(path) > self.maxlen:
                self.maxlen = len(path)
                if self.maxlen == n: return

            if len(nums) == 0:
                   return

            for i in range(len(nums)):
                if not path or nums[i] > path[-1]:
                    whats_valid = tuple([x for x in nums[i+1:] if x > nums[i]])
                    if len(whats_valid) + len(path) + 1 < self.maxlen: continue
                    dfs(whats_valid, path+(nums[i],), paths, visited)
                    if self.maxlen == n: return

            return

        paths = {}
        visited = {}
        nums = tuple(nums)
        dfs(nums, (), paths, visited)
        return self.maxlen




