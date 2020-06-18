
class Trie:
    '''
    Implement a trie with insert, search, and startsWith methods.

    Example:
    Trie trie = new Trie();
    trie.insert("apple");
    trie.search("apple");   // returns true
    trie.search("app");     // returns false
    trie.startsWith("app"); // returns true
    trie.insert("app");
    trie.search("app");     // returns true

    Note:
    You may assume that all inputs are consist of lowercase letters a-z.
    All inputs are guaranteed to be non-empty strings.
    '''
    class Node(object):
        def __init__(self, data=None):
            self.data = data
            self.children = {}
            return

    def __init__(self):
        """
        Initialize your data structure here
        """
        self.root = self.Node(object)


    def insert(self, word: str) -> None:
        """
        Inserts a word into the trie.
        """
        chars = list(word)
        p = self.root
        for c in chars:
            if c not in p.children:
                p.children[c] = self.Node()
            #print(c)
            p = p.children[c]
        p.data = word
        print(p.data)
        return None


    def search(self, word: str) -> bool:
        """
        Returns if the word is in the trie.
        """
        p = self.root
        chars = list(word)
        for c in chars:
            if c in p.children:
                #print(c)
                p = p.children[c]
            else:
                return False
        if p.data == word:
            return True
        return False


    def startsWith(self, prefix: str) -> bool:
        """
        Returns if there is any word in the trie that starts with the given prefix.
        """
        chars = list(prefix)
        p = self.root
        for c in chars:
            if c in p.children:
                p = p.children[c]
            else:
                return False
        if p.children or p.data == prefix:
            return True
        return False



# Your Trie object will be instantiated and called as such:
# obj = Trie()
# obj.insert(word)
# param_2 = obj.search(word)
# param_3 = obj.startsWith(prefix)
