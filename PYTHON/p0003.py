class Solution:
    '''
    Given a string, find the length of the longest substring without repeating characters.
    '''
    def lengthOfLongestSubstring(self, s: str) -> int:
        self.longest, current = "", ""

        for i, c in enumerate(s):
            if c not in current:
                current += c
            else:
                conflict = current.index(c)
                current = current[conflict+1:i] + c

            if len(current) > len(self.longest):
                self.longest = current

        if len(current) > len(self.longest):
            self.longest = current

        return len(self.longest)
