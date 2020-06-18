class Solution:
    '''
    Given an integer array nums, find the contiguous subarray
    (containing at least one number) which has the largest
    sum and return its sum.

    Example:
    Input: [-2,1,-3,4,-1,2,1,-5,4],
    Output: 6
    Explanation: [4,-1,2,1] has the largest sum = 6.

    Follow up:
    If you have figured out the O(n) solution, try coding another
    solution using the divide and conquer approach, which is more subtle.
    '''
    def maxSubArray(self, nums: List[int]) -> int:

        n = len(nums)
        if n == 0:
            return 0

        if n == 1:
            return nums[0]

        if all([num <=0 for num in nums]):
            return max(nums)

        # kadane algorithm
        local_best = 0
        global_best = -99999999

        for num in nums:
            local_best = max(0, local_best + num)
            global_best = max(global_best, local_best)

        return global_best


