class Solution:
    '''
    Given an array A of distinct integers sorted in ascending order, return
    the smallest index i that satisfies A[i] == i.  Return -1 if no such i exists.
    Example 1:
    Input: [-10,-5,0,3,7]
    Output: 3
    Explanation:
    For the given array, A[0] = -10, A[1] = -5, A[2] = 0, A[3] = 3, thus the output is 3.
    '''
    def fixedPoint(self, A: List[int]) -> int:
        # trivial but at times, for unknown reasons, been a mental block
        n = len(A)

        if False:
            for i in range(n):
                if A[i] == i:
                    return i
            return -1

        lo, hi = 0, n-1
        best = None
        while lo <= hi:
            mi = lo + (hi-lo)//2
            if mi == A[mi]:
                if mi == A[mi]:
                    best = mi
                lo, hi = lo, mi-1
            elif mi < A[mi]:
                lo, hi = lo, mi-1
            else:
                lo, hi = mi+1, hi
        if best is None:
            return -1

        return best
