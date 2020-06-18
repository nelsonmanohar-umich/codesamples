
class MedianFinder:
    '''
    Median is the middle value in an ordered integer list. If the size
    of the list is even, there is no middle value. So the median is the
    mean of the two middle value.

    For example,
        [2,3,4], the median is 3
        [2,3], the median is (2 + 3) / 2 = 2.5

    Design a data structure that supports the following two operations:
        void addNum(int num) - Add a integer number from the data stream to the data structure.
        double findMedian() - Return the median of all elements so far.

    Example:
        addNum(1)
        addNum(2)
        findMedian() -> 1.5
        addNum(3)
        findMedian() -> 2

    Follow up:
    If all integer numbers from the stream are between 0 and 100, how would you optimize it?
    If 99% of all integer numbers from the stream are between 0 and 100, how would you optimize it?
    '''

    def __init__(self):
        """
        initialize your data structure here.
        """
        self.n = 0
        from bisect import bisect_left
        self.items = []
        return


    def addNum(self, num: int) -> None:
        if self.n == 0:
            self.items.append(num)
            self.n += 1
            self.lo = 0
            self.hi = 0
            self.mi = (self.items[self.lo] + self.items[self.hi])/2.
        elif self.n == 1:
            if num > self.items[0]:
                 self.items.insert(1, num)
            else:
                 self.items.insert(0, num)
            self.n += 1
            self.lo = 0
            self.hi = 1
            self.mi = (self.items[self.lo] + self.items[self.hi])/2.
        else:
            ix = bisect_left(self.items, num)
            #print(ix, num, self.items)
            self.items.insert(ix, num)
            self.n += 1
            n = self.n // 2
            (self.lo, self.hi) = (n, n) if self.n % 2 else (n-1, n)
            self.mi = (self.items[self.lo] + self.items[self.hi])/2.
        #print(self.n, self.items)
        return

    def findMedian(self) -> float:
        return self.mi



# Your MedianFinder object will be instantiated and called as such:
# obj = MedianFinder()
# obj.addNum(num)
# param_2 = obj.findMedian()
