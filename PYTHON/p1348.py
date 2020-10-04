class TweetCounts:
    '''
    Implement the class TweetCounts that supports two methods:
    1. recordTweet(string tweetName, int time)
    Stores the tweetName at the recorded time (in seconds).
    2. getTweetCountsPerFrequency(string freq, string tweetName, int startTime, int endTime)

    Returns the total number of occurrences for the given tweetName per minute, hour,
    or day (depending on freq) starting from the startTime (in seconds) and ending at
    the endTime (in seconds).
    freq is always minute, hour or day, representing the time interval to get the
    total number of occurrences for the given tweetName.
    The first time interval always starts from the startTime, so the time intervals
    are [startTime, startTime + delta*1>,  [startTime + delta*1, startTime + delta*2>,
    [startTime + delta*2, startTime + delta*3>, ... , [startTime + delta*i,
    min(startTime + delta*(i+1), endTime + 1)> for some non-negative number i and
    delta (which depends on freq).
    '''
    def __init__(self):
        # not the best implementation, yet, will have to revisit, stuck with Counters
        # probably reverse semantics to Counters with respect to time in next try
        from bisect import bisect_left, bisect_right
        from functools import lru_cache
        self.tweets = {}
        self.cache = {}
        return


    def recordTweet(self, tweetName: str, time: int) -> None:
        tn, seconds = tweetName, time
        if tn not in self.tweets: self.tweets[tn] = Counter()
        self.tweets[tn] += Counter(dict([(seconds, 1)]))
        return


    def quantize(self, per_second_tweet_counter, valid_times, scale):
        t = [((timekey-self.st)//scale, per_second_tweet_counter[timekey])
             for timekey in valid_times]

        qcnts, qkeys = {}, []
        for tk, tc in t:
            if tk not in qcnts:
                qcnts[tk] = 0
                qkeys.append(tk)
            qcnts[tk] += tc

        n = (self.et - self.st + 1) // scale
        if (self.et - self.st + 1) % scale: n += 1
        res = [0] * (n)
        for tk in qkeys: res[tk] = qcnts[tk]
        return res


    def getTweetCountsPerFrequency(self, freq: str, tweetName: str, startTime: int, endTime: int) -> List[int]:
        M, H, D = 60, 60*60, 60*60*24
        self.tn, self.st, self.et = tweetName, startTime, endTime
        if self.tn not in self.tweets: return [0]

        per_second_tweet_counter = self.tweets[self.tn]

        key = (self.tn, self.st, self.et)
        if key in self.cache:
            valid_times = self.cache[key]
        else:
            valid_times = [timekey for timekey in per_second_tweet_counter
                           if self.st <= timekey <= self.et]
            self.cache[key] = valid_times

        if freq == 'minute':
            return self.quantize(per_second_tweet_counter, valid_times, M)
        if freq == 'hour':
            return self.quantize(per_second_tweet_counter, valid_times, H)
        if freq == 'day':
            return self.quantize(per_second_tweet_counter, valid_times, D)

        return [0]


# Your TweetCounts object will be instantiated and called as such:
# obj = TweetCounts()
# obj.recordTweet(tweetName,time)
# param_2 = obj.getTweetCountsPerFrequency(freq,tweetName,startTime,endTime)
