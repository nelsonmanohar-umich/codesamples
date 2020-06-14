class SummaryRanges:
    '''
    Given a data stream input of non-negative integers
    a1, a2, ..., an, ..., summarize the numbers seen so
    far as a list of disjoint intervals.

    For example, suppose the integers from the data
    stream are 1, 3, 7, 2, 6, ..., then the summary will be:
    [1, 1]
    [1, 1], [3, 3]
    [1, 1], [3, 3], [7, 7]
    [1, 3], [7, 7]
    [1, 3], [6, 7]

    Follow up:
    What if there are lots of merges and the number of disjoint
    intervals are small compared to the data stream's size?
    '''

    def __init__(self):
        """
        Initialize your data structure here.
        """
        self.points = {}
        self.intervals = {}
        return


    def inside(self, v, interval):
        start, end = interval
        if start <= v <= end: return True
        return False


    def find_interval(self, v):
        # binary search needed
        for inv in self.intervals:
            if self.inside(v, inv):
                return inv
        return []


    def addNum(self, val: int) -> None:
        l, r = val-1, val+1
        if val in self.points: return
        self.points[val] = True
        if l in self.points and r in self.points:
            left = self.find_interval(l)
            right = self.find_interval(r)
            interval = (left[0], right[1])
            del self.intervals[left]
            del self.intervals[right]
            self.intervals[interval] = True
        elif l in self.points:
            left = self.find_interval(l)
            interval = (left[0], val)
            del self.intervals[left]
            self.intervals[interval] = True
        elif r in self.points:
            right = self.find_interval(r)
            interval = (val, right[1])
            del self.intervals[right]
            self.intervals[interval] = True
        else:
            interval = (val, val)
            self.intervals[interval] = True
        return


    def getIntervals(self) -> List[List[int]]:
        return sorted(list(self.intervals.keys()))


# Your SummaryRanges object will be instantiated and called as such:
# obj = SummaryRanges()
# obj.addNum(val)
# param_2 = obj.getIntervals()
