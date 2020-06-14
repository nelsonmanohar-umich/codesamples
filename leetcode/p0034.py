class Solution:
    ''' Given an array of integers nums sorted in ascending
        order, find the starting and ending position of a
        given target value.

        Your algorithm's runtime complexity must be in the
        order of O(log n).

        If the target is not found in the array, return [-1, -1].
    '''
    def searchRange(self, nums: List[int], target: int) -> List[int]:
        if len(nums) == 0: return -1, -1
        if len(nums) == 1 and nums[0] == target: return 0, 0
        if len(nums) == 1 and nums[0] != target: return -1, -1

        from bisect import bisect_left, bisect_right

        def find(target, nums, lo, hi):
            i = bisect_left(nums, target)
            if not (0 <= i < len(nums)): return False
            if not hi > lo: return False
            if nums[hi-1] == target: return True
            return False

        lo, hi = bisect_left(nums, target), bisect_right(nums, target)
        if not find(target, nums, lo, hi):
            return -1, -1

        hi = hi - 1

        return lo, hi
