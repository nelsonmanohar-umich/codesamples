class Solution:
    # https://leetcode.com/problems/sum-of-absolute-differences-in-a-sorted-array/
    def getSumAbsoluteDifferences(self, nums: List[int]) -> List[int]:

        def psums():
            n = len(nums)
            prefix = {}
            prefix[-1] = 0
            suffix = {}
            suffix[n] = 0

            for i in range(n):
                prefix[i] = prefix[i-1] + nums[i]
            for i in range(n)[::-1]:
                suffix[i] = nums[i] + suffix[i+1]

            tot = [0] * n
            for i in range(n):
                left = (nums[i]*(i+1)) - prefix[i]
                right = (nums[i]*(n-i)) - suffix[i]
                v = abs(left) + abs(right)
                tot[i] = v
            return tot

        return psums()



        def brute_force():
            # max(quadratic on number of different elements, O(n))
            tot = []
            nums.sort()
            numset = Counter(nums)
            for num_i in numset:
                update = 0
                for num_j in numset:
                    update = update + abs(num_i - num_j) * numset[num_j]
                tot.extend([update] * numset[num_i])
            return tot
