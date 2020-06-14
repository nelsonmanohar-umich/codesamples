class Solution:
    '''
    Median is the middle value in an ordered integer list. If
    the size of the list is even, there is no middle value.
    So the median is the mean of the two middle value.

    Examples:
    [2,3,4] , the median is 3

    [2,3], the median is (2 + 3) / 2 = 2.5

    Given an array nums, there is a sliding window of size k
    which is moving from the very left of the array to the very
    right. You can only see the k numbers in the window. Each
    time the sliding window moves right by one position. Your
    job is to output the median array for each window in the
    original array.

    For example,
    Given nums = [1,3,-1,-3,5,3,6,7], and k = 3.

    Window position                Median
    ---------------               -----
    [1  3  -1] -3  5  3  6  7       1
     1 [3  -1  -3] 5  3  6  7       -1
     1  3 [-1  -3  5] 3  6  7       -1
     1  3  -1 [-3  5  3] 6  7       3
     1  3  -1  -3 [5  3  6] 7       5
     1  3  -1  -3  5 [3  6  7]      6
    Therefore, return the median sliding window as [1,-1,-1,3,5,6].

    Note:
    You may assume k is always valid, ie: k is always smaller
    than input array's size for non-empty array.

    Answers within 10^-5 of the actual value will be accepted
    as correct.
    '''
    def medianSlidingWindow(self, nums: List[int], k: int) -> List[float]:
        # very slow performing solution
        n = len(nums)

        from collections import deque

        def median(lnums):
            if k == 1:
                return lnums[0]
            if k == 2:
                return (lnums[0] + lnums[1])/2.

            lnums = sorted(lnums)
            #print(lnums, k)
            if k % 2:  # 3 5
                med = lnums[k//2]
            else: # 4
                med = (lnums[k//2-1] + lnums[k//2])/2.
            return med

        medians = []
        curr = nums[0:k]
        med = median(curr)
        for lo in range(n-k+1):
            oldval, newval = curr[0], nums[lo+k-1]
            curr = nums[lo:lo+k]

            if oldval == med and newval == med:
                medians.append(med)
                continue
            med = median(curr)
            medians.append(med)

        return medians


