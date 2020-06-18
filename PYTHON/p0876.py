# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    '''
    Given a non-empty, singly linked list with head node head, return a middle node of linked list.
    If there are two middle nodes, return the second middle node.

    Example 1:
    Input: [1,2,3,4,5]
    Output: Node 3 from this list (Serialization: [3,4,5])
    The returned node has value 3.  (The judge's serialization of this node is [3,4,5]).
    Note that we returned a ListNode object ans, such that:
    ans.val = 3, ans.next.val = 4, ans.next.next.val = 5, and ans.next.next.next = NULL.
    '''
    def middleNode(self, head: ListNode) -> ListNode:

        n = 1
        p = head
        while p:
            p = p.next
            if p:
                n += 1

        goal = n//2 + 1

        i = 1
        p = head
        while i <= goal:
            if i == goal:
                return p
            i += 1
            p = p.next

        return p
