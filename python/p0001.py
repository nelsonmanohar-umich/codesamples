class Solution:
    '''Given an array of integers, return indices of the two numbers such that they add up to a specific target.
       You may assume that each input would have exactly one solution, and you may not use the same element twice.
    '''
    def twoSum(self, nums: List[int], target: int) -> List[int]:
        n = len(nums)

        used = dict(zip(range(n), [False]*n))
        v2i = {}
        for i, v in enumerate(nums):
            if v  not in v2i:
                v2i[v] = []
            v2i[v].append(i)

        for i, v in enumerate(nums):
            goal = target - v
            used[i] = True
            if goal in v2i:
                js = [j for j in v2i[goal] if not used[j]]
                if js: return (i, js[0])
            used[i] = False

