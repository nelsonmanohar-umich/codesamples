class Solution:
    '''
    Given a set of distinct integers, nums, return all possible subsets (the power set).
    Note: The solution set must not contain duplicate subsets.
    '''
    def subsets(self, nums: List[int]) -> List[List[int]]:
        n = len(nums)
        res = []
        for i in range(2**n):
            bits = bin(i)[2:][::-1]
            itemset = [nums[j] for j, bit in enumerate(bits) if bit == "1"]
            res.append(itemset)
        return res
