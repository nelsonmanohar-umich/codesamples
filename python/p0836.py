class Solution:
    '''
    A rectangle is represented as a list [x1, y1, x2, y2], where (x1, y1)
    are the coordinates of its bottom-left corner, and (x2, y2) are the
    coordinates of its top-right corner.

    Two rectangles overlap if the area of their intersection is positive.
    To be clear, two rectangles that only touch at the corner or edges do
    not overlap.

    Given two (axis-aligned) rectangles, return whether they overlap.

    Example 1:
    Input: rec1 = [0,0,2,2], rec2 = [1,1,3,3]
    Output: true
    '''
    def isRectangleOverlap(self, rec1: List[int], rec2: List[int]) -> bool:
        x1, y1, x2, y2 = rec1
        u1, v1, u2, v2 = rec2

        def intersects(intv1, intv2):
            a, b = sorted([intv1, intv2])
            #print( a, b)
            x1, x2 = a
            y1, y2 = b
            if x1 <= y1 <= x2:
                if x1 <= y1+1 <= x2 and y1+1<= y2: return True
            if x1 <= y2 <= x2:
                if x1 <= y2-1 <= x2 and y2-1>= y1: return True
            return False

        if intersects((x1, x2), (u1, u2)) and intersects((y1, y2), (v1, v2)): return True
        return False
