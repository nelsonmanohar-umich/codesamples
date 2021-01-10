# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    '''
    Given a binary tree, imagine yourself standing on the right side of it,
    return the values of the nodes you can see ordered from top to bottom.
    '''
    def rightSideView(self, root: TreeNode) -> List[int]:
        if root is None: return []

        # bfs : choose leftmost member of each level

        def successors(parents):
            succs = []
            for p in parents:
                if p.left: succs.append(p.left)
                if p.right: succs.append(p.right)
            return succs

        parents = [root]
        children = successors(parents)
        depth = 0
        depths = {}
        depths[depth] = parents
        while children:
            depth += 1
            depths[depth] = children
            children = successors(children)

        out = []
        for d in range(len(depths)):
            out.append(depths[d][-1].val)

        return out
