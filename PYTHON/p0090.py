class Solution:
    '''
    Given a collection of integers that might contain duplicates,
    nums, return all possible subsets (the power set).
    Note: The solution set must not contain duplicate subsets.
    Example:
    Input: [1,2,2]
    Output:
    [
      [2],
      [1],
      [1,2,2],
      [2,2],
      [1,2],
      []
    ]
    '''
    def subsetsWithDup(self, nums: List[int]) -> List[List[int]]:
        n  = len(nums)
        solutions = {}
        for i in range(2**n):
            bits = format(i, "032b")[-n:]
            key = [nums[i] for i,x in enumerate(bits) if x=='1']
            key = tuple(sorted(key))
            solutions[key] = True
        keys = sorted(list(solutions.keys()))
        return keys
