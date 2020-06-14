class Solution:
    ''' Given an array nums of n integers, are there elements
        a, b, c in nums such that a + b + c = 0? Find all
        unique triplets in the array which gives the sum of zero.

        Note:
        The solution set must not contain duplicate triplets.
    '''
    def threeSum(self, nums: List[int]) -> List[List[int]]:

        n = len(nums)
        if n < 3:
            return []
        if set(nums) == set((0,)):
            return [[0,0,0]]
        nmin, nmax = min(nums), max(nums)

        dnums = dict.fromkeys(nums, 0)

        res = {}
        pairs = {}
        for i in range(n-1):
            a = nums[i]
            for j in range(i+1, n):
                b = nums[j]
                s = a+b
                if s > nmax or s < nmin: continue
                if -s not in dnums: continue
                if s not in pairs: pairs[s] = []
                pairs[s].append((i,j))

        if not pairs:
            return []
        res = {}

        for i, num in enumerate(nums):
            #if not nmin <= num <= nmax: continue
            target = -num
            if target in pairs:
                for ii,jj in pairs[target]:
                     if i not in (ii,jj):
                        key = tuple(sorted([nums[ii], nums[jj], num]))
                        res[key] = True

        return res.keys()
