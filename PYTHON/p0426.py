"""
# Definition for a Node.
class Node:
    def __init__(self, val, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right
"""

class Solution:
    '''
    Convert a Binary Search Tree to a sorted Circular Doubly-Linked List in place.
    You can think of the left and right pointers as synonymous to the predecessor
    and successor pointers in a doubly-linked list. For a circular doubly linked list,
    the predecessor of the first element is the last element, and the successor of
    the last element is the first element.

    We want to do the transformation in place. After the transformation, the left
    pointer of the tree node should point to its predecessor, and the right pointer
    should point to its successor. You should return the pointer to the smallest
    element of the linked list.
    '''
    def treeToDoublyList(self, root: 'Node') -> 'Node':
        if root is None:
            return root

        self.path = []

        def dfs(p):
            if p is None: return
            dfs(p.left)
            self.path.append(p)
            dfs(p.right)
            return

        dfs(root)

        if not self.path:
            return root

        if len(self.path) == 1:
            root.right = root
            root.left = root
            return root

        root = self.path[0]
        last = self.path[-1]
        root.left = last
        last.right = root
        n = len(self.path)
        for i in range(n):
            p = self.path[i]
            if i != n-1:
                p.right = self.path[i+1]
            else:
                p.right = root
            if i != 0:
                p.left = self.path[i-1]
            else:
                p.left = last
        return root
