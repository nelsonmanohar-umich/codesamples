class Solution:
    '''
    Given an array of n positive integers and a positive integer s, find the
    minimal length of a contiguous subarray of which the sum ≥ s. If there
    isn't one, return 0 instead.
    Example:
    Input: s = 7, nums = [2,3,1,2,4,3]
    Output: 2
    Explanation: the subarray [4,3] has the minimal length under the problem constraint.
        '''
    def minSubArrayLen(self, s: int, nums: List[int]) -> int:
        # clumsy O(n2) two pointer worst case sadly due to sum, but average case reduced due to memoization
        n = len(nums)
        nums = nums + [0]

        def sol1():
            i, j = 0, 1
            what = ()
            lsum = 0
            lmin = 99999999

            memo = {}
            while i < n or j < n:
                if what == (i,j): break
                what = (i,j)

                if (i,j) in memo:
                    lsum = memo[(i,j)]
                elif (i-1,j) in memo:
                    lsum = memo[(i-1,j)] - nums[i-1]
                elif (i, j-1) and j < n in memo:
                    lsum = memo[(i, j-1)] + nums[j]
                elif (i-1, j-1) and j < n in memo:
                    lsum = memo[(i-1,j-1)] - nums[i-1] + nums[j]
                else:
                    lsum = sum(nums[i:j])
                memo[(i,j)] = lsum

                if lsum >= s:
                    if (j-i) < lmin:
                        lmin = j - i
                    i += 1
                else:
                    j = j+1 if j < n else j
            return lmin if lmin != 99999999 else 0

        return sol1()


