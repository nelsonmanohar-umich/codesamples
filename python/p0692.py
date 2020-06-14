class Solution:
    '''
    Given a non-empty list of words, return the k most frequent elements.
    Your answer should be sorted by frequency from highest to lowest. If
    two words have the same frequency, then the word with the lower
    alphabetical order comes first.

    Example 1:
    Input: ["i", "love", "leetcode", "i", "love", "coding"], k = 2
    Output: ["i", "love"]
    Explanation: "i" and "love" are the two most frequent words.
    Note that "i" comes before "love" due to a lower alphabetical order.
    '''
    def topKFrequent(self, words: List[str], k: int) -> List[str]:
        freqs = {}
        for w in words:
            if w not in freqs: freqs[w] = 0
            freqs[w] += 1
        keys = list(sorted(freqs.keys()))
        keys.sort(key=lambda x: (-freqs[x], x), reverse=False)
        return keys[:k]
