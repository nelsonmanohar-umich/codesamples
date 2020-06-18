class Solution:
    '''
    Suppose an array sorted in ascending order is
    rotated at some pivot unknown to you beforehand.

    (i.e.,  [0,1,2,4,5,6,7] might become  [4,5,6,7,0,1,2]).

    Find the minimum element.

    You may assume no duplicate exists in the array.

    Example 1:

    Input: [3,4,5,1,2]
    Output: 1
    '''
    def findMin(self, nums: List[int]) -> int:
        n = len(nums)
        if n == 0:
            return None
        if n == 1:
            return nums[0]

        lo = 0
        hi = n-1
        vmin = 999999999999
        while lo < hi:
            mi = lo + (hi-lo)//2

            lmin = min(nums[lo], nums[mi], nums[hi])
            if nums[mi] == lmin:
                if lmin < vmin:
                    vmin = nums[mi]
                lo, hi = lo+1, hi-1
            if nums[hi] == lmin:
                if lmin < vmin:
                    vmin = nums[hi]
                lo, hi = mi+1, hi
            if nums[lo] == lmin:
                if lmin < vmin:
                    vmin = nums[lo]
                lo, hi = lo, mi-1
        return vmin
