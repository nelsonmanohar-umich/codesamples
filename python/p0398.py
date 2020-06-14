class Solution:
    '''
    Given an array of integers with possible duplicates, randomly
    output the index of a given target number. You can assume
    that the given target number must exist in the array.

    Note:
    The array size can be very large. Solution that uses too
    much extra space will not pass the judge.

    Example:
    int[] nums = new int[] {1,2,3,3,3};
    Solution solution = new Solution(nums);
    // pick(3) should return either index 2, 3, or 4 randomly. Each index should have equal probability of returning.
    solution.pick(3);
    // pick(1) should return 0. Since in the array only nums[0] is equal to 1.
    solution.pick(1);
    '''

    def __init__(self, nums: List[int]):
        # import random
        self.v2i = {}
        for i, v in enumerate(nums):
            if v not in self.v2i: self.v2i[v] = []
            self.v2i[v].append(i)
        self.nums = nums
        return


    def pick(self, target: int) -> int:
        ixs = self.v2i[target]
        return random.choice(ixs)


# Your Solution object will be instantiated and called as such:
# obj = Solution(nums)
# param_1 = obj.pick(target)
