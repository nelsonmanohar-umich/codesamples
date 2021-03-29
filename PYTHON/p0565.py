class Solution:
    # https://leetcode.com/problems/array-nesting/
    def arrayNesting(self, nums: List[int]) -> int:

        def find_chain_from(i, visited={}):
            chain = {}
            chain[i] = True
            j = nums[i]
            while j not in chain:
                chain[j] = True
                visited[j] = True
                j = nums[j]
            return len(chain)

        visited = {}
        maxlen = 0
        for i in range(len(nums)):
            if i in visited: continue
            nlen = find_chain_from(i, visited=visited)
            maxlen = max(nlen, maxlen)
        return maxlen


