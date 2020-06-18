class Solution:
    ''' Given an array nums of n integers and an integer target,
        find three integers in nums such that the sum is closest
        to target. Return the sum of the three integers. You may
        assume that each input would have exactly one solution.
    '''
    def threeSumClosest(self, nums: List[int], target: int) -> int:
        from bisect import bisect_left

        n = len(nums)
        nums.sort()
        nmin, nmax = nums[0], nums[-1]


        def change_if_better(c, closest_sum):
            this_sum = c+a+b
            if abs(target-this_sum) < abs(target-closest_sum):
                #print("new best", (a,b,c), this_sum, closest_sum)
                return this_sum
            return closest_sum


        closest_sum = -99999999999999

        #print(nums)
        for i in range(n-1):
            a = nums[i]
            #print(a)
            for j in range(i+1, n):
                b = nums[j]
                #print(b)
                goal = target - (a+b)
                #if not nmin <= goal <= nmax: continue
                ix = bisect_left(nums, goal)
                #print("***", (i, j, ix), (a, b, goal, nums[ix-1:ix+2]))

                for ic in range(ix-1, ix+1):
                    if ic in (i,j): continue
                    if not 0 <= ic < n: continue
                    c = nums[ic]
                    if goal == c:
                         #print("---", (goal, a, b), a+b+c, target)
                         return a+b+goal
                    closest_sum = change_if_better(c, closest_sum)


        #print("+++", a, b, target)
        return closest_sum


