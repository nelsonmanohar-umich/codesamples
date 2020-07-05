# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    '''
    Given a binary tree, determine if it is a valid binary search tree (BST).
    Assume a BST is defined as follows:
    The left subtree of a node contains only nodes with keys less than the node's key.
    The right subtree of a node contains only nodes with keys greater than the node's key.
    Both the left and right subtrees must also be binary search trees.
    '''
    def isValidBST(self, root: TreeNode) -> bool:
        def sol1(root):
            def dfs(p, path=[]):
                if not p: return
                dfs(p.left, path)
                path.append(p.val)
                dfs(p.right, path)
                return

            path = []
            if not root: return True
            dfs(root, path)
            h = path[0]
            for p in path[1:]:
                if p <= h: return False
                h = p
            return True


        def sol2(root):
            def is_less_subtree(p, key):
                if not p: return
                if p.val >= key.val:
                    self.valid = False
                    return
                if p.left: is_less_subtree(p.left, key)
                if p.right and self.valid: is_less_subtree(p.right, key)
                return

            def is_more_subtree(p, key):
                if not p: return
                if p.val <= key.val:
                    self.valid = False
                    return
                if p.left: is_more_subtree(p.left, key)
                if p.right and self.valid: is_more_subtree(p.right, key)
                return

            def print_tree(p, depth=0):
                if p is None: return
                print_tree(p.left, depth+1)
                print_tree(p.right, depth+1)
                return

            def dfs(p):
                if not self.valid: return
                if not p: return
                if p.left: is_less_subtree(p.left, p)
                if p.right and self.valid: is_more_subtree(p.right, p)
                if p.left and self.valid: dfs(p.left)
                if p.right and self.valid: dfs(p.right)

            self.valid = True
            dfs(root)
            return self.valid

        return sol1(root)

