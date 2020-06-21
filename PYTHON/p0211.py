class WordDictionary:
    '''
    Design a data structure that supports the following two operations:
        void addWord(word)
        bool search(word)
    search(word) can search a literal word or a regular expression string
    containing only letters a-z or .. A . means it can represent any one letter.

    Example:
        addWord("bad")
        addWord("dad")
        addWord("mad")
        search("pad") -> false
        search("bad") -> true
        search(".ad") -> true
        search("b..") -> true
    Note:
    You may assume that all words are consist of lowercase letters a-z.
    '''

    class Node(object):
        def __init__(self):
            self.children = {}
            self.word = ""
            return

    def __init__(self):
        """
        Initialize your data structure here.
        """
        self.trie = self.Node()
        return


    def addWord(self, word: str) -> None:
        """
        Adds a word into the data structure.
        """
        chars = list(word)
        p = self.trie
        for c in chars:
            if c not in p.children:
                p.children[c] = self.Node()
            p = p.children[c]
        p.word = word
        return


    def search(self, word: str) -> bool:
        """
        Returns if the word is in the data structure. A word could contain the dot character '.' to represent any one letter.
        """
        chars = list(word)
        #print("looking for", word)
        self.found = None

        def dfs_print(p):
            if p.word: print("trie", p.word)
            for c in p.children:
                dfs_print(p.children[c])
            return

        def dfs(p, chars):#, path=[]):
            if not chars:
                if p.word:
                    self.found = True
                return

            char = chars[0]
            if char != '.' and char in p.children:
                dfs(p.children[char], chars[1:]) #, path+[char])
                if self.found is not None:
                    return
            elif char != ".":
                self.found = False
                return
            else: # char == '.':
                for c in p.children:
                    dfs(p.children[c], chars[1:])#, path+[c])
                    if self.found == True: return

            return

        #dfs_print(self.trie)

        dfs(self.trie, chars)#, [])

        return self.found




# Your WordDictionary object will be instantiated and called as such:
# obj = WordDictionary()
# obj.addWord(word)
# param_2 = obj.search(word)
