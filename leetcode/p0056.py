class Solution:
    ''' Given a collection of intervals, merge all overlapping intervals.

    Example 1:
    Input: [[1,3],[2,6],[8,10],[15,18]]
    Output: [[1,6],[8,10],[15,18]]
    Explanation: Since intervals [1,3] and [2,6] overlaps, merge them into [1,6].

    Example 2:
    Input: [[1,4],[4,5]]
    Output: [[1,5]]
    Explanation: Intervals [1,4] and [4,5] are considered overlapping.
    '''
    def merge(self, intervals: List[List[int]]) -> List[List[int]]:

        n = len(intervals)
        if n == 0:
            return []
        if n == 1:
            return intervals

        intervals.sort()

        def intersects(p, s, e):
            if s <= p <= e: return True
            return False


        res = []

        for i, iv in enumerate(intervals):

            if i == 0:
                curr_start, curr_end = iv
                continue
            else:
                iv_start, iv_end = iv
                if intersects(iv_start, curr_start, curr_end):
                    curr_end = max(curr_end, iv_end)
                    continue
                else:
                    res.append([curr_start, curr_end])
                    curr_start, curr_end = iv

        res.append( [curr_start, curr_end])

        return res
