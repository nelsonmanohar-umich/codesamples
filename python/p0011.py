class Solution:
    ''' Given n non-negative integers a1, a2, ..., an , where
        each represents a point at coordinate (i, ai). n vertical
        lines are drawn such that the two endpoints of line i is
        at (i, ai) and (i, 0). Find two lines, which together
        with x-axis forms a container, such that the container
        contains the most water.

        Note: You may not slant the container and n is at least 2.
    '''
    def maxArea(self, height: List[int]) -> int:
        h = height
        n = len(height)

        i, j = 0, n-1
        maxarea = 0
        while i < j:
            hh = min(h[i],h[j])
            if hh * (j-i) > maxarea:
                maxarea = hh * (j-i)
            if hh == h[j]:
                j -= 1
            if hh == h[i]:
                i += 1
        return maxarea

        h = height
        n = len(height)
        maxarea = 0
        for i in range(n):
            for j in range(i+1, n)[::-1]:
                if h[i] <= h[j]:
                    area = (j-i)*h[i]
                    if area > max_area:
                        max_area = area
                    break
                if h[i] > h[j]:
                    area = (j-i)*h[j]
                    if area > max_area:
                        max_area = area
                    continue
        return max_area

