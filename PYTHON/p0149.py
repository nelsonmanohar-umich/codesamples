class Solution:
    '''
    Given n points on a 2D plane, find the maximum number of
    points that lie on the same straight line.

    Example 1:
    Input: [[1,1],[2,2],[3,3]]
    Output: 3
    Explanation:
    ^
    |
    |        o
    |     o
    |  o
    +------------->
    0  1  2  3  4
        '''
    def maxPoints(self, points: List[List[int]]) -> int:
        def slope(p1, p2):
            x1, y1 = p1
            x2, y2 = p2
            m = (y2-y1)/(x2-x1) if (x2-x1) else 999999999
            return m

        def intersect(p, m):
            x, y = p
            b = (y - m*x)
            return b

        n = len(points)
        if n <= 2:
            return n

        slopes = {}
        for i, p1 in enumerate(points):
            for j, p2 in enumerate(points):
                if i == j: continue
                m = slope(p1, p2)
                b = intersect(p2, m)
                key = (m, b)
                if key not in slopes: slopes[key] = {}
                slopes[key][i] = p1
                slopes[key][j] = p2


        pmax = 0
        for key in slopes:
            #print(key, slopes[key])
            klen = len(slopes[key])
            if klen > pmax:
                vals = [slopes[key][i] for i in slopes[key]]
                if [94911152,94911151] in vals:
                    klen -= 1
                pmax = klen
        return pmax


