"""
# Definition for a Node.
class Node:
    def __init__(self, x: int, next: 'Node' = None, random: 'Node' = None):
        self.val = int(x)
        self.next = next
        self.random = random
"""

class Solution:
    '''
    A linked list is given such that each node contains an additional random pointer
    which could point to any node in the list or null.  Return a deep copy of the list.

    The Linked List is represented in the input/output as a list of n nodes.
    Each node is represented as a pair of [val, random_index] where:

    val: an integer representing Node.val
    random_index: the index of the node (range from 0 to n-1) where random
    pointer points to, or null if it does not point to any node.
    '''
    def copyRandomList(self, head: 'Node') -> 'Node':
        def scan(root):
            p = root
            nodes = {}
            i = 0
            head = None
            mapping = {}
            while p:
                nodes[p] = Node(x=p.val, next=p.next, random=p.random)

                mapping[p] = Node(p.val)
                p = p.next
                i += 1
            return nodes, mapping

        nodes, mapping = scan(head)
        nodes[None] = None
        mapping[None] = None
        p = head
        while p:
            v, n, r = p.val, p.next, p.random
            pp = mapping[p]
            pp.val = v
            pp.next = mapping[n]
            pp.random = mapping[r]
            p = p.next
        return mapping[head]




