class Solution:
    '''
    Given an unsorted integer array, find the smallest missing positive integer.
     Example 1:

     Input: [1,2,0]
     Output: 3
     Example 2:

     Input: [3,4,-1,1]
     Output: 2
     Example 3:

     Input: [7,8,9,11,12]
     Output: 1
     Note:

     Your algorithm should run in O(n) time and uses constant extra space.
    '''

    def firstMissingPositive(self, nums: List[int]) -> int:
        # O(nlogn)
        v2i = {}
        vals = [x for x in nums if x > 0]

        for i, v in enumerate(vals):
            if v not in v2i:
                v2i[v] = []
            v2i[v].append(i)

        vals = sorted(list(v2i.keys()))
        vals.sort()
        n = len(vals)
        if n == 0: return 1
        if n == 1: return 2 if vals[0] == 1 else 1
        if n == 1: return min(nums[0]-1,1) if vals[0]>1 else min(nums[0]+1,1)
        if vals[0] > 1: return 1

        for i, v in enumerate(vals):
            if v-1 and v-1 not in v2i: return v-1
            if v+1 not in v2i: return v+1
            i += 1


