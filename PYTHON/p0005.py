class Solution:
    ''' Given a string s, find the longest palindromic substring in s.
        You may assume that the maximum length of s is 1000.
    '''
    def longestPalindrome(self, s: str) -> str:
        # very innefficient O(n3), an LCS approach will be tried later
        n = len(s)
        if len(set(s)) == 1: return s

        self.nmax = 1
        self.longest = s[0:1]
        for i in range(n):
            for j in range(n, i+1, -1):
                if s[i] != s[j-1]: continue
                if s[i:j] == s[i:j][::-1]:
                    if len(s[i:j]) > self.nmax:
                        self.longest = s[i:j]
                        self.nmax = len(self.longest)

        return self.longest

