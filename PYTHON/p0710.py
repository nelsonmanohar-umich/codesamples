class Solution:
    '''
    Given a blacklist B containing unique integers from [0, N), write
    a function to return a uniform random integer from [0, N) which is
    NOT in B.

    Optimize it such that it minimizes the call to system’s Math.random().

    Note:
      1 <= N <= 1000000000
      0 <= B.length < min(100000, N)
      [0, N) does NOT include N. See interval notation.
    '''
    def __init__(self, N: int, blacklist: List[int]):
        # thought about whitelist but had to look-up confirmation of such
        self.B = dict(zip(blacklist, [0] * len(blacklist)))
        self.bmax =1000
        self.nmax = 100000
        if len(self.B) > self.bmax and N < self.nmax:
            self.whitelist = [x for x in range(N) if x not in self.B]
            self.M = len(self.whitelist)
        self.N = N
        return

    def pick(self) -> int:
        if len(self.B) > self.bmax and self.N < self.nmax:
            ix = random.randint(0, self.M-1)
            return self.whitelist[ix]
        else:
            v = random.randint(0, self.N-1)
            while v in self.B:
                v = random.randint(0, self.N-1)
            return v




# Your Solution object will be instantiated and called as such:
# obj = Solution(N, blacklist)
# param_1 = obj.pick()
