class Solution:
    ''' Given an array of strings, group anagrams together.

        Example:
        Input: ["eat", "tea", "tan", "ate", "nat", "bat"],
        Output:
         [
          ["ate","eat","tea"],
          ["nat","tan"],
          ["bat"]
         ]
    '''
    def groupAnagrams(self, strs: List[str]) -> List[List[str]]:

        from collections import Counter
        from collections import defaultdict

        hashes = [(Counter(s), s) for s in strs]

        res = defaultdict(list)

        for (counter, s) in hashes:
            key = tuple(sorted(counter.items()))
            #print (key, s)
            res[key].append(s)

        output = []
        for key in res:
            output.append(res[key])

        return output


