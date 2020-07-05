# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    '''
    Given a binary tree, return the inorder traversal of its nodes' values.
    Example:
    Input: [1,null,2,3]
       1
        \
         2
        /
       3

    Output: [1,3,2]
    Follow up: Recursive solution is trivial, could you do it iteratively?
    '''
    def inorderTraversal(self, root: TreeNode) -> List[int]:
        def dfs(p, path=[]):
            if not p: return
            if p.left: dfs(p.left, path)
            path.append(p.val)
            if p.right: dfs(p.right, path)
            return
        path = []
        dfs(root, path)
        return path
