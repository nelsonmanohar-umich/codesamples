# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    '''
    We are given head, the head node of a linked list containing unique integer values.
    We are also given the list G, a subset of the values in the linked list.
    Return the number of connected components in G, where two values are connected if they appear consecutively in the linked list.
    Example 1:
    Input:
    head: 0->1->2->3
    G = [0, 1, 3]
    Output: 2
    Explanation:
    0 and 1 are connected, so [0, 1] and [3] are the two connected components.
    '''
    def numComponents(self, head: ListNode, G: List[int]) -> int:
        parents = {}
        prev, curr = None, head
        n_comps = 0
        while curr:
            if curr.val in G:
                if prev in parents:
                    parents[curr] = parents[prev]
                else:
                    parents[curr] = prev
                    n_comps += 1
            prev = curr
            curr = curr.next

        # transitive_parents = set([parents[p] for p in parents])
        # for p in parents: print (p.val, parents[p])
        # return len(transitive_parents)
        return n_comps
