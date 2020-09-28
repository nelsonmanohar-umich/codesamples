# Definition for a binary tree node.
# class TreeNode(object):
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class Codec:
    '''
    Serialization is the process of converting a data structure or object into a sequence
    of bits so that it can be stored in a file or memory buffer, or transmitted across a
    network connection link to be reconstructed later in the same or another computer
    environment.

    Design an algorithm to serialize and deserialize a binary tree. There is no restriction
    on how your serialization/deserialization algorithm should work. You just need to ensure
    that a binary tree can be serialized to a string and this string can be deserialized
    to the original tree structure.

    Clarification: The input/output format is the same as how LeetCode serializes a binary
    tree. You do not necessarily need to follow this format, so please be creative and come
    up with different approaches yourself.
    '''
    def __init__(self):
        self.records = {}
        self.nodes  = {}
        self.id = 0
        return

    def serialize(self, root):
        """Encodes a tree to a single string.
        :type root: TreeNode
        :rtype: str
        """

        def p2i(p):
            if p is None: return 0
            if p not in self.nodes:
                self.nodes[p] = self.id + 1
                self.id += 1
            return self.nodes[p]

        def dfs(p):
            if not p:
                record = (p2i(p), None, p2i(None), p2i(None))
                self.records[p] = record
                return
            record = (p2i(p), p.val, p2i(p.left), p2i(p.right))
            self.records[p] = record
            dfs(p.left)
            dfs(p.right)
            return

        dfs(root)

        out = []
        for p in self.records:
            out.append(self.records[p])
        return repr(out)


    def deserialize(self, data):
        """Decodes your encoded data to tree.
        :type data: str
        :rtype: TreeNode
        """
        out = eval(data)
        n = len(out)
        self.nodes = {}
        for i in range(n+1):
            p = TreeNode(i)
            self.nodes[i] = p

        if n == 1:
            return []

        for p_ix, p_val, p_left_ix, p_right_ix in out:
            p = self.nodes[p_ix]
            p.val = p_val
            p.left, p.right = None, None
            if p_left_ix: p.left = self.nodes[p_left_ix]
            if p_right_ix: p.right = self.nodes[p_right_ix]

        return self.nodes[1]

# Your Codec object will be instantiated and called as such:
# codec = Codec()
# codec.deserialize(codec.serialize(root))
