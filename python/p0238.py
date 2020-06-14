class Solution:
    '''
    Given an array nums of n integers where n > 1,  return an array
    output such that output[i] is equal to the product of all the
    elements of nums except nums[i].

     Example:
     Input:  [1,2,3,4]
     Output: [24,12,8,6]
     Constraint: It's guaranteed that the product of the elements of any prefix or suffix
                 of the array (including the whole array) fits in a 32 bit integer.

     Note: Please solve it without division and in O(n).

     Follow up:
     Could you solve it with constant space complexity? (The output array
     does not count as extra space for the purpose of space complexity analysis.)
    '''
    def productExceptSelf(self, nums: List[int]) -> List[int]:

        products = {}
        n = len(nums)

        products[(0,0)] = nums[0]
        products[(n-1,n-1)] = nums[-1]

        # compute all windowed products left to right
        # compute all windowed products right to left
        # get products 0:i and i+1:n

        for i in range(1,n):
            curr_key = (0,i)
            prev_key = (0,i-1)
            products[curr_key] = nums[i] * products[prev_key]

        for i in range(0,n-1)[::-1]:
            curr_key = (n-1,i)
            prev_key = (n-1,i+1)
            products[curr_key] = nums[i] * products[prev_key]

        res = [0] * n
        for i in range(n):
            if i == 0:
                left = 1
            else:
                left_key = (0,i-1)
                left = products[left_key]
            if i == n-1:
                right = 1
            else:
                right_key = (n-1,i+1)
                right = products[right_key]
            res[i] = left * right

        return res



