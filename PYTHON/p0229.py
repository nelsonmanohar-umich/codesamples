class Solution:
    '''
    Given an integer array of size n, find all elements that appear more than ⌊ n/3 ⌋ times.
    Note: The algorithm should run in linear time and in O(1) space.

    Example 1:
    Input: [3,2,3]
    Output: [3]

    Example 2:
    Input: [1,1,1,3,3,2,2,2]
    Output: [1,2]
    '''
    def majorityElement(self, nums: List[int]) -> List[int]:
        n = len(nums)
        if n == 0:
            return []
        if n == 1:
            return list(set(nums))
        if n == 2:
            return list(set(nums))

        counts = {}
        for i in range(3):
            val = nums[i]
            if val not in counts:
                counts[val] = 0
            counts[val] += 1

        n = len(nums)
        for i in range(3,n,1):
            val, target = nums[i], None
            if val in counts:
                counts[val] += 1
            elif val not in counts:
                if len(counts) == 3:
                    mincnt = min([counts[x] for x in counts])
                    target = [key for key in counts if counts[key] == mincnt][0]
                    del counts[target]
                counts[val] = 1
            #print (i, val, target, counts)
        #print(counts)
        maxval = None
        res = []
        for key in counts.keys():
            cnt = nums.count(key)
            if cnt > n/3:
                res.append(key)
        return res
        #keys = [x for x in sorted(list(counts.keys()), key=lambda x: counts[x], reverse=True) if counts[x] > n/3 or n % 3 == 0 and counts[x]> n/3]
        return res



