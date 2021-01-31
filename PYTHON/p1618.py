# """
# This is FontInfo's API interface.
# You should not implement it, or speculate about its implementation
# """
#class FontInfo(object):
#    Return the width of char ch when fontSize is used.
#    def getWidth(self, fontSize, ch):
#        """
#        :type fontSize: int
#        :type ch: char
#        :rtype int
#        """
#
#    def getHeight(self, fontSize):
#        """
#        :type fontSize: int
#        :rtype int
#        """
class Solution:
    """
    https://leetcode.com/problems/maximum-font-to-fit-a-sentence-in-a-screen/
    """
    def maxFont(self, text: str, w: int, h: int, fonts: List[int], fontInfo : 'FontInfo') -> int:
        def validate(mi):
            fs = fonts[mi]
            h_hat = fontInfo.getHeight(fs)
            w_hat = sum([fontInfo.getWidth(fs, c) for c in text])
            if w_hat <= w and h_hat <= h:
                return True
            return False

        n = len(fonts)
        lo, hi = 0, n-1
        found = -1
        while lo <= hi:
            mi = lo + (hi-lo)//2
            if validate(mi):
                lo, hi = mi+1, hi
                found = mi
            else:
                lo, hi = lo, mi-1

        return fonts[found] if found >= 0 else -1



