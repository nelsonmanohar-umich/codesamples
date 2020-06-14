class Solution:
    '''
    Given two strings s1 and s2, write a function to return
    true if s2 contains the permutation of s1. In other words,
    one of the first string's permutations is the substring
    of the second string.

    Example 1:
    Input: s1 = "ab" s2 = "eidbaooo"
    Output: True
    Explanation: s2 contains one permutation of s1 ("ba").

    Example 2:
    Input:s1= "ab" s2 = "eidboaoo"
    Output: False
    Constraints:
    The input strings only contain lower case letters.
    The length of both given strings is in range [1, 10,000].
    '''
    def checkInclusion(self, s1: str, s2: str) -> bool:

        what, where = s1, s2
        n_what, n_where = len(what), len(where)

        if n_where < n_what: return False

        from collections import Counter

        counter_what = Counter(what)
        for i in range(n_where):
            term = s2[i:i+n_what]
            if i == 0:
                moving_counter = Counter(term)
                if counter_what == moving_counter:
                    return True
            else:
                old_char = where[i-1]
                new_char = term[-1] if len(term) == n_what else ""
                moving_counter.subtract(old_char)
                if moving_counter[old_char] == 0:
                    del moving_counter[old_char]
                if new_char:
                    moving_counter.update(new_char)
                if moving_counter == counter_what:
                    return True
            #print(term, counter_what, moving_counter)
        return False


