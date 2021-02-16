class Solution:
    """
    https://leetcode.com/problems/letter-case-permutation/
    """
    def letterCasePermutation(self, S: str) -> List[str]:
        def thru_bfs():
            def get_successors(words, pos):
                succs = set()
                for w in words:
                    c = w[pos]
                    c = c.upper() if c == c.lower() else c.lower()
                    word = w[:pos] + c + w[pos+1:]
                    if word not in succs and word not in words:
                        succs.add(word)
                return succs

            n = len(S)
            words = set()
            words.add(S)
            for i in range(n):
                succs = get_successors(words, i)
                words.update(succs)
            return words

        return thru_bfs()


        def thru_powerset():
            n = len(S)
            tot = {}
            binfmt = "{0:0%sb}" % n
            for i in range(2**n):
                bits = binfmt.format(i)
                item = "".join([c.upper() if int(bit)==1 else c.lower() for bit, c in zip(bits, S)])
                tot[item] = True
            return tot.keys()

        return thru_powerset()
