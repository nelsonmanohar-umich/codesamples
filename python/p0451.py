class Solution:
    '''
    Given a string, sort it in decreasing order based on the frequency of characters.
    Example 1:
    Input: "tree"
    Output: "eert"
    Explanation:
    'e' appears twice while 'r' and 't' both appear once.
    So 'e' must appear before both 'r' and 't'. Therefore "eetr" is also a valid answer.

    '''
    def frequencySort(self, s: str) -> str:
        from collections import Counter
        counts = Counter(list(s))
        keys = list(counts.keys())
        keys.sort(key=lambda x: counts[x], reverse=True)
        return "".join( [key*counts[key] for key in keys])
