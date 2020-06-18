class Solution:
    '''
    Given a string s and a non-empty string p, find all the start indices
    of p's anagrams in s.
    Strings consists of lowercase English letters only and the length of
    both strings s and p will not be larger than 20,100.
    The order of output does not matter.

    Example 1:
    Input: s: "cbaebabacd" p: "abc"
    Output: [0, 6]
    Explanation:
       The substring with start index = 0 is "cba", which is an anagram of "abc".
       The substring with start index = 6 is "bac", which is an anagram of "abc".

    '''
    def findAnagrams(self, s: str, p: str) -> List[int]:
        from collections import Counter

        small, m = p, len(p)
        large, n = s, len(s)
        if n < m:
            return []

        if n == m:
            return [0] if Counter(small) == Counter(large) else []

        freqs_small = Counter(small)

        res = []
        for i in range(0,n,1):
            term = large[i:i+m]

            if i:
                new_char = large[i+m-1] if i+m-1 < n else ""
                old_char = large[i-1]
                moving_counter.subtract(old_char)
                if moving_counter[old_char] == 0:
                    del moving_counter[old_char]
                if new_char:
                    moving_counter.update(new_char)
            else:
                moving_counter = Counter(term)

            if moving_counter == freqs_small:
                res.append(i)

        return res
