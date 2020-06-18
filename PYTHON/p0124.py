# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    '''
    Given a non-empty binary tree, find the maximum path sum.

    For this problem, a path is defined as any sequence of
    nodes from some starting node to any node in the tree
    along the parent-child connections. The path must contain
    at least one node and does not need to go through the root.

    Example 1:
    Input: [1,2,3]
            1
           / \
          2   3
    Output: 6

    Example 2:
    Input: [-10,9,20,null,null,15,7]
        -10
        / \
       9  20
         /  \
        15   7
    Output: 42
    '''
    def maxPathSum(self, root: TreeNode) -> int:

        self.maxsum = -9999999999

        def maxsum(p, sumsofar):
            if p is None:
                return sumsofar

            left, right = 0, 0
            if p.left:
                left = maxsum(p.left, sumsofar+p.left.val)
            if p.right:
                right = maxsum(p.right, sumsofar+p.right.val)
            v = p.val
            val = max( left+v, right+v, v) #, left, right)
            if val > self.maxsum:
                #print('***', p.val, self.maxsum, val)
                self.maxsum = val
            if left + v + right > self.maxsum:
                self.maxsum = left + v + right
            return val

        def ptree(p, depth=0):
            if p is None:
                return
            print("**"*depth, p.val, p.left.val if p.left else None, p.right.val if p.right else None)
            ptree(p.left, depth+1)
            ptree(p.right, depth+1)

        #ptree(root, depth=1)
        #print('-'*32)
        ret = maxsum(root, 0)
        return self.maxsum

