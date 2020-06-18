"""
# Definition for a Node.
class Node:
    def __init__(self, val = 0, neighbors = []):
        self.val = val
        self.neighbors = neighbors
"""
class Solution:
    '''
    Given a reference of a node in a connected undirected graph.
    Return a deep copy (clone) of the graph.

    Each node in the graph contains a val (int) and a list
    (List[Node]) of its neighbors.
    class Node {
        public int val;
        public List<Node> neighbors;
    }

    '''
    def cloneGraph(self, node: 'Node') -> 'Node':

        nodes = {}
        if node is None:
            return None

        def dfs(p, nodes={}):
            if not p: return
            if p in nodes: return
            nodes[p] = True
            for neighbor in p.neighbors:
                dfs(neighbor, nodes)
            return

        dfs(node, nodes)

        mapping = {}
        # mapping[node] = Node(node.val)
        for p in nodes:
            mapping[p] = Node(p.val)

        for p in nodes:
            mapping[p].neighbors = [mapping[x] for x in p.neighbors]

        return mapping[node]
