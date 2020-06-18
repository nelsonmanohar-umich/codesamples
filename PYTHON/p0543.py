# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class Solution:
    '''
    Given a binary tree, you need to compute the length of
    the diameter of the tree. The diameter of a binary tree
    is the length of the longest path between any two nodes
    in a tree. This path may or may not pass through the root.

    Example:
    Given a binary tree
              1
             / \
            2   3
           / \
          4   5
    Return 3, which is the length of the path [4,2,1,3] or [5,2,1,3].
    Note: The length of path between two nodes is represented
    by the number of edges between them.
    '''
    def diameterOfBinaryTree(self, root: TreeNode) -> int:
        self.max_diam = 0

        def get_maxdepth(p, depth):
            if not p: return depth

            l_depth, r_depth = 0, 0
            l_depth = get_maxdepth(p.left, depth+1)
            r_depth = get_maxdepth(p.right, depth+1)

            diam_here = (l_depth-depth-1) + (r_depth-depth-1)
            if diam_here > self.max_diam:
                self.max_diam = diam_here
            return max(l_depth, r_depth)

        get_maxdepth(root, 0)
        return self.max_diam

