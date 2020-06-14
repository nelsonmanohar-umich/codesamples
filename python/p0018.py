class Solution:
    ''' Given an array nums of n integers and an integer target,
        are there elements a, b, c, and d in nums such that
        a + b + c + d = target? Find all unique quadruplets in
        the array which gives the sum of target.
    '''
    def fourSum(self, nums: List[int], target: int) -> List[List[int]]:

        solutions = {}
        n = len(nums)
        memo = {}
        for i in range(n-3):
            goal = target - nums[i]
            for j in range(i+1, n-2):
                goal = goal - nums[j]
                for k in range(j+1, n-1):
                    goal = goal - nums[k]
                    for l in range(k+1, n):
                        if nums[l] == goal:
                            solution = [nums[x] for x in (i, j, k, l)]
                            key = tuple(sorted(solution))
                            solutions[key] = True
                    goal = goal + nums[k]
                goal = goal + nums[j]
        return sorted(list(solutions.keys()))

