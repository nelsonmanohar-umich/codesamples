# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    '''
    Given the root of a tree, you are asked to find the most frequent subtree sum. The subtree sum
    of a node is defined as the sum of all the node values formed by the subtree rooted at that node
    (including the node itself). So what is the most frequent subtree sum value? If there is a tie,
    return all the values with the highest frequency in any order.
    '''
    def findFrequentTreeSum(self, root: TreeNode) -> List[int]:
        self.cnts = {}
        self.top = []
        self.max = 0


        def dfs(p):
            if not p:
                return 0

            left = dfs(p.left) if p.left else 0
            right = dfs(p.right) if p.right else 0

            pval = p.val
            psum = left + pval + right

            if psum not in self.cnts: self.cnts[psum] = 0
            self.cnts[psum] += 1

            if self.cnts[psum] > self.max:
                self.max = self.cnts[psum]
                self.top = [psum]
            elif self.cnts[psum] == self.max:
                self.top.append(psum)
            else:
                pass

            return psum

        dfs(root)

        return self.top

