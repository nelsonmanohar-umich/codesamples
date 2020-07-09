class Solution:
    '''
    Given a list of non negative integers, arrange them such that they
    form the largest number.
    Example 1:
       Input: [10,2]
       Output: "210"
    '''
    def largestNumber(self, nums: List[int]) -> str:
        if len(nums) == 0: return ""
        if len(set(nums)) == 1 and nums[0] == 0: return "0"  # ;)
        if len(nums) == 1: return str(nums[0])

        nums = [str(x) for x in nums]
        nums.sort(reverse=True)    # we need this sort, which is stable, so numbers ordered bwlow

        values = {}
        for num in nums:
            first_digit = num[0]
            if first_digit not in values: values[first_digit] = []
            values[first_digit].append(str(num))


        res = ""
        sorted_first_digits = sorted(list(values.keys()), reverse=True)
        for fd in sorted_first_digits:
            s = "".join(sorted(values[fd], key=lambda x: x.ljust(10, fd if x[-1] < fd else x[-1]), reverse=True))
            res += s

        return res



