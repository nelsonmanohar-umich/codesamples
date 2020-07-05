class Solution:
    '''
    Given an array with n objects colored red, white or blue, sort
    them in-place so that objects of the same color are adjacent,
    with the colors in the order red, white and blue.
    Here, we will use the integers 0, 1, and 2 to represent the
    color red, white, and blue respectively.
    Note: You are not suppose to use the library's sort function
    for this problem.
    Example:
       Input: [2,0,2,1,1,0]
       Output: [0,0,1,1,2,2]
    '''
    class Solution:
    def sortColors(self, nums: List[int]) -> None:
        """
        Do not return anything, modify nums in-place instead.
        """
        # generalized to arbitrary number of colors

        def sol1(nums):
            from bisect import bisect_left

            cnts = {}
            res = []
            for v in nums:
                if v not in cnts: cnts[v] = 0
                cnts[v] += 1
                ix = bisect_left(res, v)
                res.insert(ix, v)

            for i in range(len(nums)):
                nums[i] = res[i]
            return nums


        def sol2(nums):
            cnts = {}
            res = []
            for v in nums:
                if v not in cnts: cnts[v] = 0
                cnts[v] += 1

            n = len(nums)
            res = [None] * n
            start = 0
            keys = sorted(list(cnts.keys()))
            for k in keys:
                nums[start:start+cnts[k]] = [k]*cnts[k]
                start = start + cnts[k]
            return nums


        def sol3(nums):
            a = len([x for x in nums if x == 0])
            b = len([x for x in nums if x == 1])
            c = len([x for x in nums if x == 2])
            nums[0:a] = [0]*a
            nums[a:a+b] = [1]*b
            nums[a+b:] = [2]*c
            return nums

        sol1(nums)

        return nums

