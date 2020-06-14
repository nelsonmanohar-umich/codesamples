class Solution:
    '''
    Given an array nums, there is a sliding window of size k which
    is moving from the very left of the array to the very right.
    You can only see the k numbers in the window. Each time the
    sliding window moves right by one position. Return the max
    sliding window.

    Follow up:
    Could you solve it in linear time?

    Example:
    Input: nums = [1,3,-1,-3,5,3,6,7], and k = 3
    Output: [3,3,5,5,6,7]
    Explanation:

    Window position                Max
    ---------------               -----
    [1  3  -1] -3  5  3  6  7       3
     1 [3  -1  -3] 5  3  6  7       3
     1  3 [-1  -3  5] 3  6  7       5
     1  3  -1 [-3  5  3] 6  7       5
     1  3  -1  -3 [5  3  6] 7       6
     1  3  -1  -3  5 [3  6  7]      7

     Constraints:
     1 <= nums.length <= 10^5
     -10^4 <= nums[i] <= 10^4
     1 <= k <= nums.length
    '''
    def maxSlidingWindow(self, nums: List[int], k: int) -> List[int]:
        n = len(nums)
        if n == 0:
            return []

        if n <= k:
            return [max(nums)]

        if k == 1:
            return nums

        maxwin = []
        maxk0 = max(nums[0:k])
        maxk1 = max(nums[1:k])
        maxwin.append(maxk0)
        for i in range(1,n-k+1):
            maxk0 = max(maxk1, nums[i+k-1])
            if maxk0 == nums[i+k-1]:
                maxk1 = maxk0
            elif maxk0 == nums[i]:
                maxk1 = max(nums[i+1:i+k])
            maxwin.append(maxk0)
        return maxwin
