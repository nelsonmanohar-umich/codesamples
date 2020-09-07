class Codec:
    '''
    Note: This is a companion problem to the System Design problem: Design TinyURL.
    TinyURL is a URL shortening service where you enter a URL such as
    https://leetcode.com/problems/design-tinyurl and it returns a short URL such as
    http://tinyurl.com/4e9iAk.

    Design the encode and decode methods for the TinyURL service. There is no
    restriction on how your encode/decode algorithm should work. You just need
    to ensure that a URL can be encoded to a tiny URL and the tiny URL can be
    decoded to the original URL.
    '''
    def __init__(self, partition="A", uselocks=False):
        from random import randint
        import string
        self.alphabet = string.ascii_letters + string.digits + "-_"
        indexes = range(len(self.alphabet))
        self.a2i = dict(zip(self.alphabet, indexes))
        self.i2a = dict(zip(indexes, self.alphabet))
        self.uselocks = uselocks
        if self.uselocks:
            from threading import Lock
            self.updatelock = Lock()
        self.cnt = 1000
        self.partition = partition
        self.l2s = {}
        self.s2l = {}
        return


    def getkey(self, length=8, userandom=False):
        key = "%s%s" % (self.partition, self.cnt)
        if userandom:
            def _getkey(length):
                k = ""
                for c in range(length):
                    k += self.i2a[randint(0,len(self.a2i)-1)]
                return k
            key = _getkey(length)
            while key in self.s2l:
                key = key[1:] + self.i2a[randint(0,len(self.a2i)-1)]
        self.cnt += 1
        return key


    def encode(self, longUrl: str) -> str:
        """Encodes a URL to a shortened URL.
        """
        if self.uselocks:
            self.updatelock.acquire()
            # not needed at this time but to measue performance degradation (99 --> 67)
        url = longUrl
        if url not in self.l2s:
            key = self.getkey()
            self.l2s[url] = "http://%s" % key
            self.s2l["http://%s" % key] = url
        print(self.l2s[url])
        v = self.l2s[url]
        if self.uselocks:
            self.updatelock.release()
        return v


    def decode(self, shortUrl: str) -> str:
        """Decodes a shortened URL to its original URL.
        """
        if self.uselocks: self.updatelock.acquire()
        short = shortUrl
        if short in self.s2l:
            val = self.s2l[short]
        else:
            val = ""
        if self.uselocks: self.updatelock.release()
        return val


# Your Codec object will be instantiated and called as such:
# codec = Codec()
# codec.decode(codec.encode(url))
