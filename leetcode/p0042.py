class Solution:
    '''
    Given n non-negative integers representing an elevation
    map where the width of each bar is 1, compute how much
    water it is able to trap after raining.

    The above elevation map is represented by array [0,1,0,2,1,0,1,3,2,1,2,1].

    In this case, 6 units of rain water (blue section) are
    being trapped.

    Example:
        Input: [0,1,0,2,1,0,1,3,2,1,2,1]
        Output: 6
    '''
    def trap(self, height: List[int]) -> int:
        n = len(height)
        if n == 0:
            return 0

        buckets = 0
        i = 0
        j = n-1
        prev_tall = 0 #min(height)
        while i < j:
            tall = min(height[i], height[j])

            update = [tall-max(prev_tall,hh) for hh in height[i:j] if tall > hh]
            #print("***", (i, j), (left, right), update, sum(update), buckets)
            buckets += sum(update) if update else 0

            for next_i in range(i,n):
                if height[next_i] > height[i]:
                    break

            for next_j in range(i,j)[::-1]:
                if height[next_j] > height[j]:
                    break

            if height[i] == height[j]:
                i = next_i
                j = next_j
            elif height[i] < height[j]:
                i = next_i
            elif height[i] > height[j]:
                j = next_j

            prev_tall = tall

        return buckets





