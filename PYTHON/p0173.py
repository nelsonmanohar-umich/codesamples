# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class BSTIterator:
    '''
    Implement an iterator over a binary search tree (BST). Your iterator
    will be initialized with the root node of a BST.  Calling next() will
    return the next smallest number in the BST.
    '''
    def __init__(self, root: TreeNode):
        self.nodes = []
        self.dfs(root)
        self.nodes#.sort(reverse=True)
        self.ix = 0
        return

    def dfs(self, p):
        if not p:
            return
        self.dfs(p.left)
        self.nodes.append(p.val)
        self.dfs(p.right)
        return

    def next(self) -> int:
        """
        @return the next smallest number
        """
        if self.hasNext:
            v  = self.nodes[self.ix]
            self.ix += 1
            return v
        raise StopIteration

    def hasNext(self) -> bool:
        """
        @return whether we have a next smallest number
        """
        if self.ix < len(self.nodes):
            return True
        return False


# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class BSTIteratorPops:

    def __init__(self, root: TreeNode):
        self.nodes = []
        self.dfs(root)
        self.nodes.sort(reverse=True)
        return

    def dfs(self, p):
        if not p:
            return
        self.dfs(p.left)
        self.nodes.append(p.val)
        self.dfs(p.right)
        return

    def next(self) -> int:
        """
        @return the next smallest number
        """
        if self.hasNext:
            return self.nodes.pop()
        raise StopIteration

    def hasNext(self) -> bool:
        """
        @return whether we have a next smallest number
        """
        if self.nodes:
            return True
        return False



# Your BSTIterator object will be instantiated and called as such:
# obj = BSTIterator(root)
# param_1 = obj.next()
# param_2 = obj.hasNext()
