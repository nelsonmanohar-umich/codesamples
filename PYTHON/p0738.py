class Solution:
    '''
    Find the minimum length word from a given dictionary words, which has
    all the letters from the string licensePlate. Such a word is said to
    complete the given string licensePlate

    Here, for letters we ignore case. For example, "P" on the licensePlate
    still matches "p" on the word.

    It is guaranteed an answer exists. If there are multiple answers,
    return the one that occurs first in the array.

    The license plate might have the same letter occurring multiple times.
    For example, given a licensePlate of "PP", the word "pair" does not
    complete the licensePlate, but the word "supper" does.

    Example 1:
    Input: licensePlate = "1s3 PSt", words = ["step", "steps", "stripe", "stepple"]
    Output: "steps"
    Explanation: The smallest length word that contains the letters "S", "P", "S", and "T".
    Note that the answer is not "step", because the letter "s" must occur in the word twice.
    Also note that we ignored case for the purposes of comparing whether a letter exists in the word.
    '''
    def shortestCompletingWord(self, licensePlate: str, words: List[str]) -> str:
        from collections import Counter
        from string import ascii_lowercase

        n = len(words)
        lp_counts = Counter([x.lower() for x in licensePlate if x.lower() in ascii_lowercase])
        lp_chars = set(list(lp_counts.keys()))

        def is_match(w1c, w2c):
            for char in w1c:
                if w2c[char] >= w1c[char]: continue
                return False
            return True

        cands = []
        for w in words:
            w_chars = set(list(w))
            if lp_chars <= w_chars:
                cands.append(w)

        cands = [w for w in cands if is_match(lp_counts, Counter(w))]
        mlen = 9999999
        for w in cands:
            if len(w) < mlen:
                mlen = len(w)
                mcand = w
        return mcand

