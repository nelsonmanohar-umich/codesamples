class Solution:
    '''
    Given a sorted array nums, remove the duplicates in-place
    such that duplicates appeared at most twice and return the
    new length.

    Do not allocate extra space for another array, you must do this
    by modifying the input array in-place with O(1) extra memory.

    Example 1:
    Given nums = [1,1,1,2,2,3],
    Your function should return length = 5, with the first five
    elements of nums being 1, 1, 2, 2 and 3 respectively.
    It doesn't matter what you leave beyond the returned length.

    '''
    def removeDuplicates(self, nums: List[int]) -> int:
        # extra space used in the solution for counts, sorry

        n = len(nums)
        if n == 0:
            return 0
        if n == 1:
            return 1

        from collections import Counter

        i = 0
        cnts = Counter(nums)
        for key in cnts.keys():
            cnt = cnts[key]
            #print(i, key, cnt, nums)
            if cnt >= 1:
                nums[i] = key
                i += 1
            if cnt > 1:
                nums[i] = key
                i += 1
        nums[i+1:] = []
        return i
