class HitCounter:
    '''
    Design a hit counter which counts the number of hits received in the past 5 minutes.
    Each function accepts a timestamp parameter (in seconds granularity) and you may
    assume that calls are being made to the system in chronological order (ie, the
    timestamp is monotonically increasing). You may assume that the earliest timestamp
    starts at 1.

    It is possible that several hits arrive roughly at the same time.

    Example:
    HitCounter counter = new HitCounter();
    // hit at timestamp 1.
    counter.hit(1);
    // hit at timestamp 2.
    counter.hit(2);
    // hit at timestamp 3.
    counter.hit(3);
    // get hits at timestamp 4, should return 3.
    counter.getHits(4);
    // hit at timestamp 300.
    counter.hit(300);
    // get hits at timestamp 300, should return 4.
    counter.getHits(300);
    // get hits at timestamp 301, should return 3.
    counter.getHits(301);

    Follow up:
    What if the number of hits per second could be very large? Does your design scale?
    '''
    def __init__(self):
        """
        Initialize your data structure here.
        """
        from bisect import bisect_right
        self.cache = {}
        self.buffer = []
        self.keys = {}
        return


    def hit(self, timestamp: int) -> None:
        """
        Record a hit.
        @param timestamp - The current timestamp (in seconds granularity).
        """
        if not timestamp in self.cache: self.cache[timestamp] = 0
        self.buffer.append(timestamp)
        self.cache[timestamp] += 1
        return


    def getHits(self, timestamp: int) -> int:
        """
        Return the number of hits in the past 5 minutes.
        @param timestamp - The current timestamp (in seconds granularity).
        """
        # pivot = bisect_right(self.buffer, max(timestamp-300,0))
        # return len(self.buffer[pivot:])

        # original solution to the small array size problem wrt possibly counter updates with value different than 1
        start, end = max(0,timestamp-300)+1, timestamp+1
        keys = [t for t in self.cache if t in range(start, end)]
        vals = [self.cache[t] for t in keys]
        return sum(vals)




# Your HitCounter object will be instantiated and called as such:
# obj = HitCounter()
# obj.hit(timestamp)
# param_2 = obj.getHits(timestamp)


# Your HitCounter object will be instantiated and called as such:
# obj = HitCounter()
# obj.hit(timestamp)
# param_2 = obj.getHits(timestamp)
