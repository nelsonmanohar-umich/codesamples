class Solution:
    """
    https://leetcode.com/problems/random-pick-with-weight/
    You are given an array of positive integers w where w[i] describes the weight of ith index (0-indexed).
    We need to call the function pickIndex() which randomly returns an integer in the range [0, w.length - 1].
    pickIndex() should return the integer proportional to its weight in the w array. For example,
    for w = [1, 3], the probability of picking the index 0 is 1 / (1 + 3) = 0.25 (i.e 25%) while the
    probability of picking the index 1 is 3 / (1 + 3) = 0.75 (i.e 75%).

    More formally, the probability of picking index i is w[i] / sum(w).
    """

    def __init__(self, w: List[int]):
        self.w = w
        self.starts = []
        lo = 0
        for i in range(len(w)):
            lo, hi = lo, lo + w[i]
            self.starts.append(lo)
            lo = hi
        self.n = sum(w)
        self.starts.append(self.n-1)
        return


    def pickIndex(self) -> int:
        if self.n == 1: return 0
        pick = random.randint(0,self.n-1)
        ix = bisect.bisect_right(self.starts, pick)
        if ix == len(self.starts): return len(self.w) - 1
        return ix-1



# Your Solution object will be instantiated and called as such:
# obj = Solution(w)
# param_1 = obj.pickIndex()
