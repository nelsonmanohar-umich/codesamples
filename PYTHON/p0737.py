class Solution:
    """
    https://leetcode.com/problems/sentence-similarity-ii/
    """
    def areSentencesSimilarTwo(self, words1: List[str], words2: List[str], pairs: List[List[str]]) -> bool:
        n1, n2 = len(words1), len(words2)
        if n1 != n2: return False

        mapping = {}
        for w1, w2 in pairs:
            if w1 not in mapping: mapping[w1] = set()
            mapping[w1].update([w2])
            if w2 not in mapping: mapping[w2] = set()
            mapping[w2].update([w1])

        self.visited = {}

        def dfs(w, visited=set()):
            if w in visited: return
            visited.update([w])
            for v in mapping[w]:
                if v not in visited:
                    dfs(v, visited)
            return

        done = {}
        for w in mapping:
            done[w] = False

        for w in mapping:
            if done[w]: continue
            visited = set()
            dfs(w, visited)
            visited.update([w])
            self.visited[w] = visited
            for v in mapping[w]:
                self.visited[v] = visited
                done[v] = True
            done[w] = True

        for w1, w2 in zip(words1, words2):
            if w1 == w2: continue
            if w1 not in self.visited: return False
            if w2 not in self.visited[w1]: return False
        return True


