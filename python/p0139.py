class Solution:
    '''
    Given a non-empty string s and a dictionary wordDict containing
    a list of non-empty words, determine if s can be segmented into
    a space-separated sequence of one or more dictionary words.

    Note:
    The same word in the dictionary may be reused multiple times in
    the segmentation.

    You may assume the dictionary does not contain duplicate words.

    Example 1:
    Input: s = "leetcode", wordDict = ["leet", "code"]
    Output: true
    Explanation: Return true because "leetcode" can be segmented as "leet code".
    '''
    def wordBreak(self, s: str, wordDict: List[str]) -> bool:

        self.cansplit = False
        def explore(s, neighbor, path=[], visited={}):
            if not s:
                self.cansplit = True
                return

            key = (s, neighbor)
            if key in visited: return
            visited[key] = True

            neighbors = [w for w in wordDict if s.startswith(w)]

            for neighbor in neighbors:
                k = len(neighbor)
                explore(s[k:], neighbor, path+[neighbor], visited)
                if self.cansplit: return

            return

        visited = {}
        path = []

        for w in wordDict:
            if s.startswith(w):
                k = len(w)
                explore(s[k:], w, path+[w], visited)
                if self.cansplit: return self.cansplit
        return self.cansplit
