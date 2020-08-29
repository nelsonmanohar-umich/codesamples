class CombinationIterator:
    '''
    '''
    def __init__(self, characters: str, combinationLength: int):
        self.k = combinationLength
        chars = characters
        self.combs = {}
        if False:
            self.keys = self.powerset(chars)
        else:
            self.combine(0, chars, "")
            self.keys = list(self.combs.keys()) # a quirk that works though if should be ordereddict or sorted
        self.i = 0
        self.n = len(self.keys)
        return


    def powerset(self, chars):
        # brute force to see what level of performance it gets
        n = len(chars)
        bitformat = "{:0%sb}" % n
        items = [bitformat.format(i)[::-1] for i in range(2**n) if bin(i).count('1') == self.k]
        i2c = dict(zip(range(n), chars))
        words = ["".join([i2c[i] for i, b in enumerate(bits) if b == '1']) for bits in items]
        self.words = {}
        for w in words: self.words[w] = True
        self.keys = sorted(list(self.words.keys()))
        return self.keys


    def combine(self, ix, chars, path):
        if path in self.combs: return
        n = len(chars)
        if len(path) > self.k: return
        if len(path) == self.k:
            self.combs[path] = True
            return
        for i in range(ix, n):
            self.combine(i, chars[:i] + chars[i+1:], path+chars[i])
        return


    def next(self) -> str:
        if self.hasNext():
            ret = self.keys[self.i]
            self.i += 1
            return ret
        return


    def hasNext(self) -> bool:
        if self.i < self.n:
            return True
        return False



# Your CombinationIterator object will be instantiated and called as such:
# obj = CombinationIterator(characters, combinationLength)
# param_1 = obj.next()
# param_2 = obj.hasNext()
